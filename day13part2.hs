#!/usr/bin/env stack
{- stack
  script
  --resolver lts-9.0
  --package hspec
  --package hspec-core
  --package containers
-}
import Data.Char (isDigit)
import Data.Either (partitionEithers)
import Test.Hspec (SpecWith, describe, hspec, it, shouldBe)

data Firewall = Firewall
  { fwDepth :: Int
  , fwRange :: Int
  } deriving (Show, Eq)

mkFirewall :: Int -> Int -> Firewall
mkFirewall depth range = Firewall {fwDepth = depth, fwRange = range}

readInt :: String -> Either String Int
readInt s =
  if all isDigit s
    then Right (read s)
    else Left "Expected digits"

readSingleFirewall :: String -> Either String Firewall
readSingleFirewall s =
  case words s of
    [sDepth, sRange] -> do
      depth <- readInt $ stripColon sDepth
      range <- readInt sRange
      return $ mkFirewall depth range
    _ -> Left "Expected atleast 2 words in string"
  where
    stripColon = takeWhile (/= ':')

readFirewalls :: [String] -> Either [String] [Firewall]
readFirewalls ls =
  case partitionEithers $ readSingleFirewall <$> ls of
    ([], firewalls) -> Right firewalls
    (errs, _) -> Left errs

modForRange :: Int -> Int
modForRange n = (n - 1) * 2

modForFw :: Firewall -> Int
modForFw = modForRange . fwRange

validFw :: Firewall -> Int -> Bool
validFw fw offset = ((offset + fwDepth fw) `mod` modForFw fw) /= 0

flawlessOffset :: [Firewall] -> Int -> Bool
flawlessOffset [] _ = True
flawlessOffset (fw:fws) offset = validFw fw offset && flawlessOffset fws offset

solve :: [Firewall] -> Int
solve fws = head $ filter (flawlessOffset fws) [0 ..]

input :: IO [Firewall]
input = do
  file <- readFile "day13input.txt"
  let eithers = readFirewalls $ lines file
  return $ either (error . mconcat) id eithers

example :: [String]
example = ["0: 3", "1: 2", "4: 4", "6: 4"]

tests :: SpecWith ()
tests = do
  describe "modForRange" $
    it "works" $ modForRange <$> [2, 3, 4, 5] `shouldBe` [2, 4, 6, 8]
  describe "solve" $
    it "works with example" $
    solve <$> readFirewalls example `shouldBe` Right 10

main :: IO ()
main = do
  inp <- input
  print $ length inp
  hspec tests
  putStrLn $ "solution: " ++ show (solve inp) ++ " expected 3905748"
