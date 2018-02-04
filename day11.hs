#!/usr/bin/env stack
{- stack
  script
  --resolver lts-9.0
  --package hspec
  --package hspec-core
  --package split
-}
import Data.Either (lefts, rights)
import Data.List.Split (splitOn)
import Specs (specFromExamples, specItem)
import Test.Hspec (Spec, SpecWith, describe, hspec, it, shouldBe)

data Direction
  = N
  | NE
  | SE
  | S
  | SW
  | NW
  deriving (Show, Eq)

type Path = [Direction]

readDirection :: String -> Either String Direction
readDirection s =
  case s of
    "n" -> Right N
    "s" -> Right S
    "ne" -> Right NE
    "se" -> Right SE
    "sw" -> Right SW
    "nw" -> Right NW
    x -> Left $ "'" ++ x ++ "' is an unknown direction"

readPath :: String -> Either [String] Path
readPath s =
  case lefts paths of
    [] -> Right $ rights paths
    ls -> Left ls
  where
    paths = readDirection <$> splitOn "," s

solve :: Path -> Path
solve = undefined

input :: IO Path
input = do
  file <- readFile "day11input.txt"
  let eitherPaths = readPath $ head $ lines file
  return $ either (error . mconcat) id eitherPaths

tests :: SpecWith ()
tests = describe "x" $ it "y" $ "x" `shouldBe` "y"

main :: IO ()
main = do
  inp <- input
  print $ length inp
  hspec tests
  putStrLn $ "solution: " ++ show (solve inp)
