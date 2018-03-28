#!/usr/bin/env stack
{- stack
  script
  --resolver lts-9.0
  --package hspec
  --package hspec-core
-}
{-# LANGUAGE RecordWildCards #-}

{- finishes best compiled, i.e. stack build && stack exec day17part2 -}
import Test.Hspec (SpecWith, describe, hspec, it, shouldBe)

data Spinlock = Spinlock
  { index1 :: Maybe Int
  , position :: !Int
  } deriving (Eq, Show)

evolve :: Int -> (Int, Int) -> Spinlock -> Spinlock
evolve steps (i, limit) sl@Spinlock {..} =
  if i > limit
    then sl
    else evolve steps (i + 1, limit) Spinlock {position = next, index1 = i1}
  where
    l = i
    next = ((position + steps) `mod` l) + 1
    i1 =
      if next == 1
        then Just i
        else index1

initial :: Spinlock
initial = Spinlock {index1 = Nothing, position = 0}

solve :: Int -> Int -> Maybe Int
solve steps n = index1 spinlock
  where
    spinlock = evolve steps (1, n) initial

tests :: SpecWith ()
tests =
  describe "evolve" $ do
    it "one step" $
      evolve 3 (1, 1) initial `shouldBe`
      Spinlock {index1 = Just 1, position = 1}
    it "with example" $
      evolve 3 (1, 9) initial `shouldBe`
      Spinlock {index1 = Just 9, position = 1}

input :: Int
input = 369

main :: IO ()
main = do
  hspec tests
  putStrLn $ "solution: " ++ show (solve input (50 * 1000 * 1000))
