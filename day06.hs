#!/usr/bin/env stack
{- stack
  script
  --resolver lts-9.0
  --package hspec
-}
import Data.List (sortOn)
import Test.Hspec
import Test.Hspec.QuickCheck

indexed :: (Enum b, Num b) => [a] -> [(a, b)]
indexed xs = zip xs [0 ..]

replaceNth :: Int -> (a -> a) -> [a] -> [a]
replaceNth n op (x:xs)
  | n == 0 = op x : xs
  | otherwise = x : replaceNth (n - 1) op xs

nextIndex :: Int -> [Int] -> Int
nextIndex index list = (index + 1) `mod` length list

addFromIndex :: Int -> Int -> [Int] -> [Int]
addFromIndex _ 0 res = res
addFromIndex index remainder res =
  addFromIndex
    (nextIndex index res)
    (remainder - 1)
    (replaceNth index (+ 1) res)

redistribute :: [Int] -> [Int]
redistribute xs =
  addFromIndex (nextIndex donorIndex xs) donorSize $
  replaceNth donorIndex (const 0) xs
  where
    (donorSize, donorIndex) = head $ sortOn (negate . fst) $ indexed xs
    -- maximumBy?

helper :: [Int] -> [[Int]] -> [[Int]]
helper current previous =
  if current `elem` previous
    then previous
    else helper (redistribute current) (previous ++ [current])

solve :: [Int] -> Int
solve initial = length $ helper initial []

tests = do
  describe "nextIndex" $ do
    let input = [0, 0, 0, 0]
    let examples = [(1, 2), (2, 3), (3, 0), (0, 1)]
    it "works with example of length 4" $
      fmap (\(x, _) -> nextIndex x input) examples `shouldBe` snd <$> examples
  describe "addFromIndex" $ do
    prop "stops" $ \xs -> addFromIndex 0 0 xs `shouldBe` xs
    it "works for 1 on 0" $ addFromIndex 0 1 [0, 0] `shouldBe` [1, 0]
    it "works for 1 on 1" $ addFromIndex 1 1 [0, 0] `shouldBe` [0, 1]
    it "works for 2 on 0" $ addFromIndex 0 2 [0, 0] `shouldBe` [1, 1]
    it "works for 2 on 1" $ addFromIndex 1 2 [0, 0] `shouldBe` [1, 1]
    it "works for tricky example" $
      addFromIndex 1 3 [0, 1, 2, 3] `shouldBe` [0, 2, 3, 4]
  describe "redistribute" $ do
    let examples =
          [ ([1, 0, 0, 0], [0, 1, 0, 0])
          , ([0, 2, 7, 0], [2, 4, 1, 2])
          , ([2, 4, 1, 2], [3, 1, 2, 3])
          , ([3, 1, 2, 3], [0, 2, 3, 4])
          ]
    it "works with examples" $
      (redistribute . fst) <$> examples `shouldBe` fmap snd examples
  describe "solve" $ it "works with example" $ solve [0, 2, 7, 0] `shouldBe` 5

input :: [Int]
input = [11, 11, 13, 7, 0, 15, 5, 5, 4, 4, 1, 1, 7, 1, 15, 11]

main :: IO ()
main = do
  hspec tests
  putStrLn $ "solution: " ++ show (solve input)
