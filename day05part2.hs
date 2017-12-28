#!/usr/bin/env stack
{- stack
  script
  --resolver lts-9.0
  --package hspec
  --package vector
  --package primitive
-}
{-# LANGUAGE TypeFamilies #-}

{- BIG NOTE: this does not really complete without ghc -O2 which is not achievable with script
   Run this with stack build && stack exec day05part2
-}
import qualified Control.Monad.Primitive as P
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as MV

replaceNth ::
     P.PrimMonad m
  => Int
  -> (Int -> Int)
  -> MV.MVector (P.PrimState m) Int
  -> m ()
replaceNth n op v = do
  now <- MV.read v n
  MV.write v n (op now)

current v pointer = MV.read v pointer

increment :: P.PrimMonad m => MV.MVector (P.PrimState m) Int -> Int -> m ()
increment v p = do
  c <- current v p
  let change =
        if c >= 3
          then (\x -> x - 1)
          else (+ 1)
  replaceNth p change v

helper :: P.PrimMonad m => MV.MVector (P.PrimState m) Int -> Int -> Int -> m Int
helper v p count = do
  c <- current v p
  let next = p + c
  if next >= MV.length v
    then return $ count + 1
    else do
      _ <- increment v p
      helper v next $ count + 1

solve :: P.PrimMonad m => [Int] -> m Int
solve instructions = do
  v <- V.thaw $ V.fromList instructions
  helper v 0 0

--tests =
--  describe "solve" $
--  it "works with example" $ solve [0, 3, 0, 1, -3] `shouldBe` 10
main :: IO ()
main = do
  ex <- solve [0, 3, 0, 1, -3]
  print $ ex
  inputLines <- lines <$> readFile "./day05input.txt"
  let input = map (\s -> read s :: Int) inputLines
  solved <- solve input
  putStrLn $ "solution: " ++ show solved
