#!/usr/bin/env stack
{- stack
  script
  --resolver lts-9.0
  --package hspec
  --package vector
-}
import qualified Data.Vector.Primitive as V
import Data.Vector.Primitive (Vector(..), (!), (//))
import Test.Hspec

replaceNth :: V.Prim a => Int -> (a -> a) -> Vector a -> Vector a
replaceNth n op v = v // [(n, new)]
  where
    new = op $ v ! n

data State = State
  { sInstructions :: Vector Int
  , sPointer :: Int
  }

current :: State -> Int
current s = sInstructions s ! sPointer s

mkState :: [Int] -> State
mkState ins = State {sInstructions = V.fromList ins, sPointer = 0}

increment :: State -> State
increment state = state {sInstructions = newIns}
  where
    newIns = replaceNth (sPointer state) (+ 1) (sInstructions state)

helper :: State -> Int -> Int
helper state count =
  if next >= (V.length . sInstructions) state
    then count + 1
    else helper ((increment state) {sPointer = next}) $ count + 1
  where
    next = sPointer state + current state

solve :: [Int] -> Int
solve instructions = helper (mkState instructions) 0

tests =
  describe "solve" $
  it "works with example" $ solve [0, 3, 0, 1, -3] `shouldBe` 5

main :: IO ()
main = do
  hspec tests
  inputLines <- lines <$> readFile "./day05input.txt"
  let input = map (\s -> read s :: Int) inputLines
  putStrLn $ "solution: " ++ show (solve input)
