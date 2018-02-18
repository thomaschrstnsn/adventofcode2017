#!/usr/bin/env stack
{- stack
  script
  --resolver lts-9.0
  --package hspec
  --package hspec-core
  --package containers
-}
import Data.Bits (testBit)
import KnotHash (knotHash)
import Specs (specFromExamples, specItem)
import Test.Hspec (SpecWith, describe, hspec, it, shouldBe)

data Square
  = Free
  | Used
  deriving (Show, Eq)

free :: Square -> Bool
free Free = True
free _ = False

used :: Square -> Bool
used = not . free

input :: String
input = "amgozmfv"

hexCharAsInt :: Char -> Word
hexCharAsInt '0' = 0
hexCharAsInt '1' = 1
hexCharAsInt '2' = 2
hexCharAsInt '3' = 3
hexCharAsInt '4' = 4
hexCharAsInt '5' = 5
hexCharAsInt '6' = 6
hexCharAsInt '7' = 7
hexCharAsInt '8' = 8
hexCharAsInt '9' = 9
hexCharAsInt 'a' = 10
hexCharAsInt 'b' = 11
hexCharAsInt 'c' = 12
hexCharAsInt 'd' = 13
hexCharAsInt 'e' = 14
hexCharAsInt 'f' = 15
hexCharAsInt _ = error "Not a hex char"

boolAsSquare :: Bool -> Square
boolAsSquare True = Used
boolAsSquare False = Free

hexCharAsSquares :: Char -> [Square]
hexCharAsSquares c =
  boolAsSquare . testBit (hexCharAsInt c) <$> reverse [0 .. 3]

bitsAsSquares :: String -> [Square]
bitsAsSquares xs = bitAsSquare <$> xs
  where
    bitAsSquare '0' = Free
    bitAsSquare '1' = Used
    bitAsSquare _ = error "undefined 'bitvalue'"

squaresForHash :: String -> [[Square]]
squaresForHash s = rowSquares
  where
    indexStrings = (\i -> s ++ "-" ++ show i) <$> [0 :: Int .. 127]
    hashes = knotHash <$> indexStrings
    rowSquares = concatMap hexCharAsSquares <$> hashes

solve :: String -> Int
solve s = length $ filter used $ concat $ squaresForHash s

hexCharAsSquaresSpec :: SpecWith ()
hexCharAsSquaresSpec =
  specFromExamples
    [ ('0', "0000")
    , ('1', "0001")
    , ('2', "0010")
    , ('d', "1101")
    , ('e', "1110")
    , ('f', "1111")
    ]
    (\(inp, expected) ->
       specItem (show inp ++ " should be: " ++ expected) $
       hexCharAsSquares inp `shouldBe` bitsAsSquares expected)

tests :: SpecWith ()
tests = do
  describe "hexCharAsSquares" hexCharAsSquaresSpec
  describe "hex as squares" $
    it "with example" $
    concatMap hexCharAsSquares "a0c20170" `shouldBe`
    bitsAsSquares "10100000110000100000000101110000"
  describe "solve" $ it "with example" $ solve "flqrgnkx" `shouldBe` 8108

main :: IO ()
main = do
  hspec tests
  putStrLn $ "solution: " ++ show (solve input)
