#!/usr/bin/env stack
{- stack
  script
  --resolver lts-9.0
  --package hspec
  --package hspec-core
  --package vector
-}
{-# LANGUAGE RecordWildCards #-}

import Data.Char (isAlpha)
import Data.Maybe (catMaybes)
import qualified Data.Vector as V
import Data.Vector (Vector, (!))
import Specs (specFromExamples, specItem)
import Test.Hspec (SpecWith, describe, hspec, it, shouldBe)

type VVector a = Vector (Vector a)

type Field = Char

type Input = VVector Char

shouldCollect :: Field -> Bool
shouldCollect = isAlpha

canMoveOnto :: Field -> Bool
canMoveOnto x = x /= ' ' || shouldCollect x

data Direction
  = North
  | South
  | West
  | East
  deriving (Show, Eq)

data State = State
  { line :: Int
  , column :: Int
  , direction :: Direction
  } deriving (Show, Eq)

continue :: Input -> State -> Maybe State
continue inp s@State {..} =
  if maxLine > line' && line' >= 0 && maxColumn > column' && column' >= 0
    then Just $ s {line = line', column = column'}
    else Nothing
  where
    maxLine = V.length inp
    maxColumn = V.length (inp ! 0)
    (ld, cd) =
      case direction of
        North -> (-1, 0)
        South -> (1, 0)
        West -> (0, -1)
        East -> (0, 1)
    (line', column') = (line + ld, column + cd)

item :: Input -> State -> Char
item vvx s = (vvx ! line s) ! column s

findStart :: Input -> State
findStart inp =
  head $
  filter (canMoveOnto . item inp) $
  (\c -> State {line = 0, column = c, direction = South}) <$> [0 ..]

tryHead :: [a] -> Maybe a
tryHead [] = Nothing
tryHead (x:_) = Just x

switchDirection :: Input -> State -> Maybe State
switchDirection inp state =
  tryHead $
  filter (canMoveOnto . item inp) $
  catMaybes $ (\d -> continue inp $ state {direction = d}) <$> orthogonals
  where
    orthogonals =
      case direction state of
        North -> [East, West]
        South -> [East, West]
        East -> [North, South]
        West -> [North, South]

follow :: Input -> State -> Int -> Int
follow inp state res =
  case continueState of
    Just state' ->
      if canMoveOnto $ item inp state'
        then follow inp state' res'
        else case switchDirection inp state of
               Just state'' -> follow inp state'' res'
               Nothing -> res'
    Nothing -> res'
  where
    res' = res + 1
    continueState = continue inp state

solve :: Input -> Int
solve inp = follow inp (findStart inp) 0

fromListOfLists :: [[a]] -> VVector a
fromListOfLists xss = V.fromList $ V.fromList <$> xss

readInput :: String -> Input
readInput = fromListOfLists . lines

input :: IO Input
input = readInput <$> readFile "day19input.txt"

example :: String
example =
  unlines
    [ "     |          "
    , "     |  +--+    "
    , "     A  |  C    "
    , " F---|----E|--+ "
    , "     |  |  |  D "
    , "     +B-+  +--+ "
    , "                "
    ]

itemSpec :: SpecWith ()
itemSpec =
  specFromExamples
    [((0, 0), ' '), ((5, 0), '|'), ((1, 3), 'F')]
    (\(inp@(col, lin), expected) ->
       specItem ("item at " ++ show inp ++ " should be: " ++ show expected) $
       item
         (readInput example)
         State {line = lin, column = col, direction = South} `shouldBe`
       expected)

canMoveOntoSpec :: SpecWith ()
canMoveOntoSpec =
  specFromExamples
    [('F', True), (' ', False), ('+', True), ('|', True), ('-', True)]
    (\(inp, expected) ->
       specItem
         (show inp ++ " canMoveOnto should be " ++ show expected)
         (canMoveOnto inp `shouldBe` expected))

tests :: SpecWith ()
tests = do
  describe "solve" $
    it "for example gives expected trail" $
    (solve . readInput) example `shouldBe` 38
  describe "findStart" $
    it "works with example" $
    (findStart . readInput) example `shouldBe`
    State {line = 0, column = 5, direction = South}
  describe "item" itemSpec
  describe "canMoveOnto" canMoveOntoSpec

main :: IO ()
main = do
  hspec tests
  inp <- input
  putStrLn $ "solution: " ++ show (solve inp)
