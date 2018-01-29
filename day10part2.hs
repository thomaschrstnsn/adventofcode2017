#!/usr/bin/env stack
{- stack
  script
  --resolver lts-9.0
  --package hspec
  --package hspec-core
  --package parsec
-}
import Data.Bits (xor)
import Data.Char (ord)
import Numeric (showHex)
import Test.Hspec (SpecWith, describe, hspec, it, shouldBe)
import Test.Hspec.Core.Spec (fromSpecList, specItem)

asciiToInt :: String -> [Int]
asciiToInt s = ord <$> s

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

hash :: Int -> [Int] -> [Int] -> [Int]
hash rounds circlist lengths = go 0 0 circlist lengths (rounds - 1)
  where
    go _ _ cl [] 0 = cl
    go index skipsize cl [] r = go index skipsize cl lengths (r - 1)
    go index skipsize circlist (l:ls) r =
      go (index + skipsize + l) (1 + skipsize) (rotate circlist index l) ls r

input :: String
input = "102,255,99,252,200,24,219,57,103,2,226,254,1,0,69,216"

sparseToDense :: [Int] -> [Int]
sparseToDense [] = []
sparseToDense xs = xored : sparseToDense rest
  where
    (these, rest) = splitAt 16 xs
    xored = foldl xor 0 these

hexadecimal :: [Int] -> String
hexadecimal = foldMap toHex
  where
    toHex i =
      let hex = showHex i ""
      in if length hex == 1
           then '0' : hex
           else hex

extraLength :: [Int]
extraLength = [17, 31, 73, 47, 23]

solve :: String -> String
solve s = hexadecimal $ sparseToDense $ hash 64 [0 .. 255] (il ++ extraLength)
  where
    il = asciiToInt s

specFromExamples examples builder = fromSpecList $ map builder examples

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

solveSpec =
  specFromExamples
    [ ("", "a2582a3a0e66e6e86e3812dcb672a272")
    , ("AoC 2017", "33efeb34ea91902bb2f59c9920caa6cd")
    , ("1,2,3", "3efbe78a8d82f29979031a4aa0b16a9d")
    , ("1,2,4", "63960835bcdc130f0b66d7ff4f6a5a8e")
    ]
    (\(inp, expected) ->
       specItem ("\"" ++ inp ++ "\" yields " ++ expected) $
       solve inp `shouldBe` expected)

tests :: SpecWith ()
tests = do
  describe "rotate" rotateSpec
  describe "hash" $
    it "works as example" $
    hash 1 [0 .. 4] [3, 4, 1, 5] `shouldBe` [3, 4, 2, 1, 0]
  describe "asciiToInt" $
    it "works" $ asciiToInt "1,2,3" `shouldBe` [49, 44, 50, 44, 51]
  describe "denseToSparse" $ do
    it "works as example" $
      sparseToDense [65, 27, 9, 1, 4, 3, 40, 50, 91, 7, 6, 0, 2, 5, 68, 22] `shouldBe`
      [64]
    it "works with more than one 16 group" $
      sparseToDense [65, 27, 9, 1, 4, 3, 40, 50, 91, 7, 6, 0, 2, 5, 68, 22, 28] `shouldBe`
      [64, 28]
  describe "hexadecimal" $
    it "works" $ hexadecimal [64, 7, 255] `shouldBe` "4007ff"
  describe "solve" solveSpec

main :: IO ()
main = do
  hspec tests
  putStrLn $ "solution: " ++ show (solve input)
