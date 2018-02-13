#!/usr/bin/env stack
{- stack
  script
  --resolver lts-9.0
  --package hspec
  --package hspec-core
  --package containers
-}
import KnotHash (knotHash)
import Specs (specFromExamples, specItem)
import Test.Hspec (Spec, SpecWith, describe, hspec, it, shouldBe)

input :: String
input = "102,255,99,252,200,24,219,57,103,2,226,254,1,0,69,216"

solve :: String -> String
solve = undefined

tests :: SpecWith ()
tests = do
  describe "desc" $ it "does" $ "x" `shouldBe` "y"

main :: IO ()
main = do
  hspec tests
  putStrLn $ "solution: " ++ show (solve input)
