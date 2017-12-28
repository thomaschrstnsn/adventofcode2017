#!/usr/bin/env stack
{- stack
  script
  --resolver lts-9.0
  --package hspec
-}
import Test.Hspec

replaceNth :: Int -> (a -> a) -> [a] -> [a]
replaceNth n op (x:xs)
  | n == 0 = op x : xs
  | otherwise = x : replaceNth (n - 1) op xs

data State = State
  { sInstructions :: [Int]
  , sLength :: Int
  , sPointer :: Int
  }

current :: State -> Int
current s = sInstructions s !! sPointer s

mkState :: [Int] -> State
mkState ins = State {sInstructions = ins, sLength = length ins, sPointer = 0}

increment :: State -> State
increment state = state {sInstructions = newIns}
  where
    newIns = replaceNth (sPointer state) (+ 1) (sInstructions state)

helper :: State -> Int -> Int
helper state count =
  if next >= sLength state
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
  putStrLn $ "solution: " ++ show (input)
