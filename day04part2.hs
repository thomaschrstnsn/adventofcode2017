#!/usr/bin/env stack
{- stack
  script
  --resolver lts-9.0
  --package hspec
-}
import Data.List (nub, sort)
import Test.Hspec

validPassphrase :: String -> Bool
validPassphrase s = noDuplicates && noAnagrams
  where
    ws = words s
    distinct = nub ws
    noDuplicates = length distinct == length ws
    orderedWords = sort <$> ws
    noAnagrams = length (nub orderedWords) == length ws

tests =
  describe "validPassphrase" $ do
    let expected =
          [ ("abcde fghij", True)
          , ("abcde xyz ecdab", False)
          , ("a ab abc abd abf abj", True)
          , ("iiii oiii ooii oooi oooo", True)
          , ("oiii ioii iioi iiio", False)
          ]
    it "works" $
      (validPassphrase . fst) <$> expected `shouldBe` snd <$> expected

main :: IO ()
main = do
  hspec tests
  total <- lines <$> readFile "./day04input.txt"
  print $ length $ filter validPassphrase total
