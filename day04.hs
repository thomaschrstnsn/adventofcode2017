#!/usr/bin/env stack
{- stack
  script
  --resolver lts-9.0
  --package hspec
-}
import Data.List (nub)
import Test.Hspec

validPassphrase :: String -> Bool
validPassphrase s = length distinct == length ws
  where
    ws = words s
    distinct = nub ws

tests =
  describe "validPassphrase" $ do
    let expected =
          [ ("aa bb cc dd ee", True)
          , ("aa bb cc dd aa", False)
          , ("aa bb cc dd aaa", True)
          ]
    it "works" $
      (validPassphrase . fst) <$> expected `shouldBe` snd <$> expected

main :: IO ()
main = do
  hspec tests
  total <- lines <$> readFile "./day04input.txt"
  print $ length $ filter validPassphrase total
