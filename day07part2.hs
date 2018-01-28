#!/usr/bin/env stack
{- stack
  script
  --resolver lts-9.0
  --package hspec
  --package containers
-}
import Data.Char (isDigit)
import Data.Foldable (Foldable(..), find)
import Data.Function (on)
import Data.List (maximumBy, nub)
import Data.Maybe (catMaybes, fromMaybe)
import Data.Monoid (Sum(..), getSum)
import Data.Ord (compare)
import Data.Tree
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

root :: Tree a -> a
root (Node x _) = x

type ProgramNode = Program Int

type SummedProgramNode = Program (Int, Int)

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
      Node (spec2node spec) (buildSubtree <$> (sAbove . snd) spec)
      where
        spec = findSpec n

sumOfAbove :: Tree ProgramNode -> Int
sumOfAbove t = getSum $ foldMap (Sum . snd) t

summedTree :: Tree ProgramNode -> Tree SummedProgramNode
summedTree (Node (n, weight) cs) =
  Node (n, (weight, weightAbove)) (fmap summedTree cs)
  where
    weightAbove = getSum $ foldMap (Sum . sumOfAbove) cs

unbalancedAtLevel :: [Tree SummedProgramNode] -> [(String, Int)]
unbalancedAtLevel levelNodes =
  map (\(n, w, dw) -> (n, w - dw)) $
  filter (\(_, _, dw) -> dw /= 0) $
  map (\(n, w, wa) -> (n, w, w + wa - mostFreq)) namesAndWeights
  where
    nameAndWeight (Node (n, (w, wa)) _) = [(n, w, wa)]
    namesAndWeights = foldMap nameAndWeight levelNodes
    weightFreq w =
      length $ filter (\(_, w', wa) -> w' + wa == w) namesAndWeights
    mostFreq =
      maximumBy (compare `on` weightFreq) $
      map (\(_, w, wa) -> w + wa) namesAndWeights

findUnbalanced :: Tree SummedProgramNode -> [(String, Int)]
findUnbalanced (Node _ cs) = unbalancedAtLevel cs ++ foldMap findUnbalanced cs

solve :: [ProgramSpec] -> Maybe [(String, Int)]
solve inp = (findUnbalanced . summedTree) <$> buildTree inp

example :: IO [ProgramSpec]
example = specsFromFile "day07example.txt"

filterTree :: (a -> Bool) -> Tree a -> Maybe (Tree a)
filterTree f (Node n cs) =
  if matchesNode || matchesChildren
    then Just (Node n matchingChildren)
    else Nothing
  where
    matchesNode = f n
    matchingChildren = catMaybes $ fmap (filterTree f) cs
    matchesChildren = not $ null matchingChildren

tests = do
  describe "foldable tree" $ do
    let t = Node "hej" [Node "med" [], Node "dig" []]
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
      solve ex `shouldBe` Just [("ugml", 60)]

input :: IO [ProgramSpec]
input = specsFromFile "day07input.txt"

main :: IO ()
main = do
  ex <- example
  let exTree = buildTree ex
  putStrLn $ maybe "empty" (drawTree . fmap show . summedTree) exTree
  hspec tests
  inp <- input
  putStrLn $ "solution: " ++ show (solve inp)
  let inpTree = summedTree <$> buildTree inp
  let filtered =
        inpTree >>=
        filterTree (\(n, _) -> n `elem` ["qjvtm", "boropxd", "cwwwj"])
  putStrLn "filtered"
  putStrLn $ maybe "empty" (drawTree . fmap show) filtered
