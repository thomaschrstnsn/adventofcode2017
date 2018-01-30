#!/usr/bin/env stack
{- stack
  script
  --resolver lts-9.0
  --package hspec
  --package hspec-core
-}
import Specs (specFromExamples, specItem)
import Test.Hspec (Spec, SpecWith, describe, hspec, it, shouldBe)

rotate :: [Int] -> Int -> Int -> [Int]
rotate xs index len = result
  where
    l = length xs
    ls = cycle xs
    reversed = reverse $ take len $ drop index ls
    spliced = drop (len + index) ls
    total = take l (reversed ++ spliced)
    index' = index `mod` l
    (t, h) = splitAt (l - index') total
    result = h ++ t

hash :: [Int] -> [Int] -> [Int]
hash = go 0 0
  where
    go _ _ cl [] = cl
    go index skipsize circlist (l:ls) =
      go (index + skipsize + l) (1 + skipsize) (rotate circlist index l) ls

input :: [Int]
input = [102, 255, 99, 252, 200, 24, 219, 57, 103, 2, 226, 254, 1, 0, 69, 216]

prod :: [Int] -> Int
prod (x:(y:_)) = x * y
prod _ = error "not enough elements for product"

solve :: [Int] -> Int
solve = prod . hash [0 .. 255]

rotateSpec :: Spec
rotateSpec =
  specFromExamples
    [ ([0, 1, 2, 3, 4], 0, 3, [2, 1, 0, 3, 4])
    , ([0, 1, 2, 3, 4], 3, 4, [4, 3, 2, 1, 0])
    , ([2, 1, 0, 3, 4], 3, 4, [4, 3, 0, 1, 2])
    , ([4, 3, 0, 1, 2], 3, 1, [4, 3, 0, 1, 2])
    , ([4, 3, 0, 1, 2], 1, 5, [3, 4, 2, 1, 0])
    ]
    (\(inp, index, len, expected) ->
       specItem
         ("(" ++
          show inp ++
          ", " ++
          show index ++ ", " ++ show len ++ ") yields: " ++ show expected) $
       rotate inp index len `shouldBe` expected)

tests :: SpecWith ()
tests = do
  describe "rotate" rotateSpec
  describe "hash" $
    it "works as example" $
    hash [0 .. 4] [3, 4, 1, 5] `shouldBe` [3, 4, 2, 1, 0]
  describe "prod" $ it "works as example" $ prod [3, 4, 2, 1, 0] `shouldBe` 12

main :: IO ()
main = do
  hspec tests
  putStrLn $ "solution: " ++ show (solve input)
