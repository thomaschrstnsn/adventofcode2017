#!/usr/bin/env stack
{- stack
  script
  --resolver lts-9.0
  --package hspec
-}
import Data.Char (isDigit)
import Data.Foldable (Foldable(..), find)
import Data.List (nub)
import Data.Maybe (catMaybes)
import Test.Hspec (describe, hspec, it, shouldBe)
import Test.Hspec.QuickCheck

data ProgramSpec = ProgramSpec
  { psName :: String
  , psWeight :: Int
  , psAbove :: [String]
  } deriving (Show)

readWeight :: String -> Int
readWeight input = read $ takeWhile isDigit $ drop 1 input

readAboves :: [String] -> [String]
readAboves inputs = removeTrailingComma <$> inputs
  where
    removeTrailingComma = takeWhile (',' /=)

readSpec :: String -> Maybe ProgramSpec
readSpec input = do
  let ws = words input
  let name = head ws
  let weight = readWeight $ ws !! 1
  let above =
        if length ws >= 3
          then readAboves $ drop 3 ws
          else []
  return ProgramSpec {psName = name, psWeight = weight, psAbove = above}

readSpecs :: String -> [ProgramSpec]
readSpecs s = catMaybes $ readSpec <$> lines s

specsFromFile :: String -> IO [ProgramSpec]
specsFromFile fn = readSpecs <$> readFile fn

data Tree a
  = Empty
  | Node a
         [Tree a]
  | Leaf a
  deriving (Show, Eq)

instance Foldable Tree where
  foldMap _ Empty = mempty
  foldMap f (Leaf x) = f x
  foldMap f (Node x cs) = f x `mappend` mconcat (fmap (foldMap f) cs)

nonRoots :: [ProgramSpec] -> [String]
nonRoots xs = nub $ concatMap psAbove xs

buildTree :: [ProgramSpec] -> Tree String
buildTree ps =
  case root of
    Just x -> Leaf $ psName x
    Nothing -> Empty
  where
    nonRoots' = nonRoots ps
    root = find (\x -> (psName x) `notElem` nonRoots') ps

solve = buildTree

example :: IO [ProgramSpec]
example = specsFromFile "day07example.txt"

tests = do
  describe "foldable tree" $ do
    let t = Node "hej" [Leaf "med", Leaf "dig"]
    it "foldMap" $ foldMap (\x -> [x]) t `shouldBe` ["hej", "med", "dig"]
  describe "nonRoots" $
    it "works" $ do
      ex <- example
      nonRoots ex `shouldBe`
        [ "ktlj"
        , "cntj"
        , "xhth"
        , "pbga"
        , "havc"
        , "qoyq"
        , "ugml"
        , "padx"
        , "fwft"
        , "gyxo"
        , "ebii"
        , "jptl"
        ]
  describe "buildTree" $
    it "with example" $ do
      ex <- example
      buildTree ex `shouldBe` Leaf "tknk"

input :: IO [ProgramSpec]
input = specsFromFile "day07input.txt"

main :: IO ()
main = do
  ex <- example
  print ex
  hspec tests
  inp <- input
  putStrLn $ "solution: " ++ show (solve inp)
