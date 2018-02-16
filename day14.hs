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
import Specs (specFromExamples, specItem)

data Square
  = Free
  | Used

free :: Square -> Bool
free Free = True
free _ = False

used :: Square -> Bool
used = not . free

input :: String
input = "102,255,99,252,200,24,219,57,103,2,226,254,1,0,69,216"

charAsSquares :: Char -> [Square]
charAsSquares = undefined

bitsAsSquares :: String -> [Square]
bitsAsSquares xs = bitAsSquare <$> xs
  where
    bitAsSquare '0' = Free
    bitAsSquare '1' = Used
    bitAsSquare _ = error "undefined 'bitvalue'"

solve :: String -> String
solve = undefined

charAsSquaresSpec = specFromExamples [('0', "0000"), ('1', "0001"), ('e', "1110"), ('f', "1111")] (\(inp, expected) -> charAsSquares inp `shouldBe`)

tests :: SpecWith ()
tests = do
  describe "charAsSquares" charAsSquaresSpec
  describe "desc" $ it "does" $ "x" `shouldBe` "y"

main :: IO ()
main = do
  hspec tests
  putStrLn $ "solution: " ++ show (solve input)
