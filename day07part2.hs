#!/usr/bin/env stack
{- stack
  script
  --resolver lts-9.0
  --package hspec
-}
import Data.Char (isDigit)
import Data.Foldable (Foldable(..), find)
import Data.Function (on)
import Data.List (maximumBy, nub)
import Data.Maybe (catMaybes, fromMaybe)
import Data.Monoid (Sum(..), getSum)
import Data.Ord (compare)
import Test.Hspec (describe, hspec, it, shouldBe)

type Program a = (String, a)

name :: Program a -> String
name = fst

data Spec = Spec
  { sWeight :: Int
  , sAbove :: [String]
  } deriving (Show)

type ProgramSpec = Program Spec

readWeight :: String -> Int
readWeight x = read $ takeWhile isDigit $ drop 1 x

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
  return (name, Spec {sWeight = weight, sAbove = above})

readSpecs :: String -> [ProgramSpec]
readSpecs s = catMaybes $ readSpec <$> lines s

specsFromFile :: String -> IO [ProgramSpec]
specsFromFile fn = readSpecs <$> readFile fn

data Tree a
  = Node a
         [Tree a]
  | Leaf a
  deriving (Show, Eq)

root :: Tree a -> a
root (Node x _) = x
root (Leaf x) = x

instance Foldable Tree where
  foldMap f (Leaf x) = f x
  foldMap f (Node x cs) = f x `mappend` mconcat (fmap (foldMap f) cs)

instance Functor Tree where
  fmap f (Node x cs) = Node (f x) (fmap (fmap f) cs)
  fmap f (Leaf x) = Leaf (f x)

type ProgramNode = Program Int

spec2node :: ProgramSpec -> ProgramNode
spec2node (n, s) = (n, sWeight s)

nonRoots :: [ProgramSpec] -> [String]
nonRoots xs = nub $ concatMap (sAbove . snd) xs

buildTree :: [ProgramSpec] -> Maybe (Tree ProgramNode)
buildTree ps = fmap (buildSubtree . name) root
  where
    nonRoots' = nonRoots ps
    root = find (\x -> name x `notElem` nonRoots') ps
    findSpec n =
      fromMaybe
        (error $ "could not find node: '" ++ n ++ "'")
        (find (\x -> fst x == n) ps)
    buildSubtree n =
      case (sAbove . snd) spec of
        [] -> Leaf $ spec2node spec
        xs -> Node (spec2node spec) (fmap buildSubtree xs)
      where
        spec = findSpec n

sumOfAbove :: Tree ProgramNode -> Int
sumOfAbove t = getSum $ foldMap (Sum . snd) t

summedTree :: Tree ProgramNode -> Tree ProgramNode
summedTree (Node (n, weight) cs) =
  Node (n, weightAbove + weight) (fmap summedTree cs)
  where
    weightAbove = getSum $ foldMap (Sum . sumOfAbove) cs
summedTree (Leaf x) = Leaf x

unbalancedAtLevel :: [Tree ProgramNode] -> [(String, Int)]
unbalancedAtLevel levelNodes =
  map (\(n, _) -> (n, mostFreq)) $
  filter (\(_, dw) -> dw /= 0) $
  map (\(n, w) -> (n, w - mostFreq)) namesAndWeights
  where
    nameAndWeight (Node (n, w) _) = [(n, w)]
    nameAndWeight (Leaf (n, w)) = [(n, w)]
    namesAndWeights = foldMap nameAndWeight levelNodes
    weightFreq w = length $ filter (\(_, w') -> w' == w) namesAndWeights
    mostFreq = maximumBy (compare `on` weightFreq) $ map snd namesAndWeights

findUnbalanced :: Tree ProgramNode -> [(String, Int)]
findUnbalanced (Node _ cs) = unbalancedAtLevel cs ++ foldMap findUnbalanced cs
findUnbalanced (Leaf _) = []

solve :: [ProgramSpec] -> Maybe [(String, Int)]
solve inp = (findUnbalanced . summedTree) <$> buildTree inp

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
      root <$> buildTree ex `shouldBe` Just ("tknk", 41)
  describe "solve" $
    it "works with example" $ do
      ex <- example
      solve ex `shouldBe` Just [("ugml", 243)]

input :: IO [ProgramSpec]
input = specsFromFile "day07input.txt"

main :: IO ()
main = do
  ex <- example
  print ex
  hspec tests
  inp <- input
  putStrLn $ "solution: " ++ show (solve inp)
