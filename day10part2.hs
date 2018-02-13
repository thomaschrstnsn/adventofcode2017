#!/usr/bin/env stack
{- stack
  script
  --resolver lts-9.0
  --package hspec
  --package hspec-core
  --package containers
-}
import qualified Data.Sequence as Seq
import KnotHash
import Specs (specFromExamples, specItem)
import Test.Hspec (Spec, SpecWith, describe, hspec, it, shouldBe)

input :: String
input = "102,255,99,252,200,24,219,57,103,2,226,254,1,0,69,216"

solve :: String -> String
solve = knotHash

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
       rotate (Seq.fromList inp) index len `shouldBe` Seq.fromList expected)

solveSpec :: Spec
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
    hash 1 (Seq.fromList [0 .. 4]) (Seq.fromList [3, 4, 1, 5]) `shouldBe`
    Seq.fromList [3, 4, 2, 1, 0]
  describe "asciiToInt" $
    it "works" $ asciiToInt "1,2,3" `shouldBe` Seq.fromList [49, 44, 50, 44, 51]
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
