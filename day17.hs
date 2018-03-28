#!/usr/bin/env stack
{- stack
  script
  --resolver lts-9.0
  --package hspec
  --package hspec-core
-}
{-# LANGUAGE RecordWildCards #-}

import Test.Hspec (SpecWith, describe, hspec, it, shouldBe)

data Spinlock = Spinlock
  { buffer :: [Int]
  , position :: Int
  } deriving (Eq, Show)

evolve :: Int -> [Int] -> Spinlock -> Spinlock
evolve _ [] sl = sl
evolve steps (x:xs) Spinlock {..} =
  evolve steps xs Spinlock {position = next, buffer = before ++ [x] ++ after}
  where
    l = length buffer
    next = ((position + steps) `mod` l) + 1
    (before, after) = splitAt next buffer

initial :: Spinlock
initial = Spinlock {buffer = [0], position = 0}

solve :: Int -> Int -> Int
solve steps n = head $ snd $ splitAt (position spinlock + 1) (buffer spinlock)
  where
    spinlock = evolve steps [1 .. n] initial

tests :: SpecWith ()
tests =
  describe "evolve" $ do
    it "one step" $
      evolve 3 [1] initial `shouldBe` Spinlock {buffer = [0, 1], position = 1}
    it "with example" $
      evolve 3 [1 .. 9] initial `shouldBe`
      Spinlock {buffer = [0, 9, 5, 7, 2, 4, 3, 8, 6, 1], position = 1}

input :: Int
input = 369

main :: IO ()
main = do
  hspec tests
  putStrLn $ "solution: " ++ show (solve input 2017)
