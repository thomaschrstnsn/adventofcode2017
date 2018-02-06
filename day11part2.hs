#!/usr/bin/env stack
{- stack
  script
  --resolver lts-9.0
  --package hspec
  --package hspec-core
  --package split
-}
import Data.Either (lefts, rights)
import Data.List.Split (splitOn)
import Specs (specFromExamples, specItem)
import Test.Hspec (SpecWith, describe, hspec, shouldBe)

data Direction
  = N
  | NE
  | SE
  | S
  | SW
  | NW
  deriving (Show, Eq)

type Path = [Direction]

readDirection :: String -> Either String Direction
readDirection s =
  case s of
    "n" -> Right N
    "s" -> Right S
    "ne" -> Right NE
    "se" -> Right SE
    "sw" -> Right SW
    "nw" -> Right NW
    x -> Left $ "'" ++ x ++ "' is an unknown direction"

readPath :: String -> Either [String] Path
readPath s =
  case lefts paths of
    [] -> Right $ rights paths
    ls -> Left ls
  where
    paths = readDirection <$> splitOn "," s

-- https://www.redblobgames.com/grids/hexagons/#coordinates-cube
data CubeCoordinate = CubeCoordinate
  { ccX :: Int
  , ccY :: Int
  , ccZ :: Int
  } deriving (Show, Eq)

coord :: (Int, Int, Int) -> CubeCoordinate
coord (x, y, z) = CubeCoordinate {ccX = x, ccY = y, ccZ = z}

instance Monoid CubeCoordinate where
  mempty = CubeCoordinate {ccX = 0, ccY = 0, ccZ = 0}
  mappend a b = CubeCoordinate {ccX = x, ccY = y, ccZ = z}
    where
      add f = f a + f b
      x = add ccX
      y = add ccY
      z = add ccZ

directionAsCoord :: Direction -> CubeCoordinate
directionAsCoord N = coord (0, 1, -1)
directionAsCoord NE = coord (1, 0, -1)
directionAsCoord SE = coord (1, -1, 0)
directionAsCoord S = coord (0, -1, 1)
directionAsCoord SW = coord (-1, 0, 1)
directionAsCoord NW = coord (-1, 1, 0)

-- https://www.redblobgames.com/grids/hexagons/#distances
hexDistance :: CubeCoordinate -> CubeCoordinate -> Int
hexDistance a b = (absDiff ccX + absDiff ccY + absDiff ccZ) `div` 2
  where
    absDiff f = abs (f a - f b)

solve :: Path -> Int
solve p = snd $ foldl helper (mempty, 0) $ directionAsCoord <$> p
  where
    helper :: (CubeCoordinate, Int) -> CubeCoordinate -> (CubeCoordinate, Int)
    helper (position, maxDist) delta =
      (newPos, max (hexDistance mempty newPos) maxDist)
      where
        newPos = mappend position delta

input :: IO Path
input = do
  file <- readFile "day11input.txt"
  let eitherPaths = readPath $ head $ lines file
  return $ either (error . mconcat) id eitherPaths

solveSpec :: SpecWith ()
solveSpec =
  specFromExamples
    [ ("ne,ne,ne", 3)
    , ("ne,ne,sw,sw", 2)
    , ("ne,ne,s,s", 2)
    , ("se,sw,se,sw,sw", 3)
    ]
    (\(inp, expected) ->
       specItem
         ("maximum distance from (0,0,0) to any point on path: '" ++
          inp ++ "' should be: " ++ show expected) $
       (solve <$> readPath inp) `shouldBe` Right expected)

tests :: SpecWith ()
tests = describe "solve" solveSpec

main :: IO ()
main = do
  inp <- input
  print $ length inp
  hspec tests
  putStrLn $ "solution: " ++ show (solve inp)
