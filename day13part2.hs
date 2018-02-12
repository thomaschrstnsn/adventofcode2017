#!/usr/bin/env stack
{- stack
  script
  --resolver lts-9.0
  --package hspec
  --package hspec-core
  --package containers
-}
import Control.Arrow ((&&&))
import Data.Char (isDigit)
import Data.Either (partitionEithers)
import Test.Hspec (SpecWith, describe, hspec, it, shouldBe)

data Direction
  = Up
  | Down
  deriving (Show, Eq)

data Firewall = Firewall
  { fwDepth :: Int
  , fwPosition :: Int
  , fwRange :: Int
  , fwDirection :: Direction
  } deriving (Show, Eq)

mkFirewall :: Int -> Int -> Firewall
mkFirewall depth range =
  Firewall
  {fwDepth = depth, fwPosition = 0, fwRange = range, fwDirection = Down}

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
      return
        Firewall
        {fwRange = range, fwDepth = depth, fwPosition = 0, fwDirection = Down}
    _ -> Left "Expected atleast 2 words in string"
  where
    stripColon = takeWhile (/= ':')

readFirewalls :: [String] -> Either [String] [Firewall]
readFirewalls ls =
  case partitionEithers $ readSingleFirewall <$> ls of
    ([], firewalls) -> Right firewalls
    (errs, _) -> Left errs

switch :: Direction -> Direction
switch Up = Down
switch Down = Up

tick :: Firewall -> Firewall
tick fw@Firewall {fwPosition = pos, fwRange = range, fwDirection = direction} =
  fw {fwPosition = newPos, fwDirection = newDirection}
  where
    newDirection =
      if (pos + 1) == range || (pos == 0 && direction /= Down)
        then switch direction
        else direction
    newPos =
      if newDirection == Down
        then pos + 1
        else pos - 1

solve :: [Firewall] -> Int
solve fws' = length $ takeWhile (not . helper 0) fails
  where
    helper :: Int -> [Firewall] -> Bool
    helper _ [] = True
    helper depth (fw:fws) =
      if fwIsForDepth && fwPosition fw == 0
        then False
        else helper (depth + 1) (map tick nextFws)
      where
        fwIsForDepth = fwDepth fw == depth
        nextFws =
          if fwIsForDepth
            then fws
            else fw : fws
    fails = iterate (map tick) fws'

input :: IO [Firewall]
input = do
  file <- readFile "day13input.txt"
  let eithers = readFirewalls $ lines file
  return $ either (error . mconcat) id eithers

example :: [String]
example = ["0: 3", "1: 2", "4: 4", "6: 4"]

tests :: SpecWith ()
tests = do
  describe "tick" $
    it "moves up and down" $
    map (fwPosition &&& fwDirection) (take 8 $ iterate tick (mkFirewall 0 3)) `shouldBe`
    [ (0, Down)
    , (1, Down)
    , (2, Down)
    , (1, Up)
    , (0, Up)
    , (1, Down)
    , (2, Down)
    , (1, Up)
    ]
  describe "solve" $
    it "works with example" $
    solve <$> readFirewalls example `shouldBe` Right 10

main :: IO ()
main = do
  inp <- input
  print $ length inp
  hspec tests
  putStrLn $ "solution: " ++ show (solve inp)
