#!/usr/bin/env stack
{- stack
  script
  --resolver lts-9.0
  --package hspec
  --package QuickCheck
-}
import Data.Function (fix)
import Data.List (sort)
import Test.Hspec
import Test.Hspec.QuickCheck

{-
 37  36  35  34  33  32  31
 38  17  16  15  14  13  30
 39  18   5   4   3  12  29
 40  19   6   1   2  11  28
 41  20   7   8   9  10  27
 42  21  22  23  24  25  26
 43  44  45  46  47  48  49
-}
-- given a level returns the dimensions of the memory at the level
dimension :: Int -> Int
dimension 1 = 1
dimension n = 2 + dimension (n - 1)

size :: Int -> Int
size n = dimension n ^ (2 :: Int)

findLevelForCell :: Int -> Int
findLevelForCell c = head $ filter (\d -> size d >= c) [1 ..]

cellsInLevel :: Int -> [Int]
cellsInLevel 1 = [1]
cellsInLevel n = [size (n - 1) + 1 .. size n]

data Side
  = East
  | North
  | West
  | South
  deriving (Show, Eq, Ord)

data CellSides = CellSides
  { sLeft :: [Int]
  , sRight :: [Int]
  , sTop :: [Int]
  , sBottom :: [Int]
  } deriving (Show)

sidesForLevel :: Int -> CellSides
sidesForLevel level =
  CellSides {sLeft = left, sRight = right, sBottom = bottom, sTop = top}
  where
    cells = cellsInLevel level
    dim = dimension level
    right = take dim $ last cells : cells
    cellsForNextSide lastSide = take dim $ dropWhile (last lastSide /=) cells
    top = cellsForNextSide right
    left = cellsForNextSide top
    bottom = cellsForNextSide left

sidesForCell :: Int -> [Side]
sidesForCell 1 = []
sidesForCell n = map fst $ filter (cellIsInSide . (\f -> f sides) . snd) rtlb
  where
    level = findLevelForCell n
    sides = sidesForLevel level
    cellIsInSide side = n `elem` side
    rtlb = [(East, sRight), (North, sTop), (West, sLeft), (South, sBottom)]

indexed :: (Enum b, Num b) => [a] -> [(a, b)]
indexed xs = zip xs [0 ..]

cellOffsetInSide :: Int -> Side -> Int
cellOffsetInSide cell side = cellOffset
  where
    selector =
      case side of
        West -> reverse . sLeft
        East -> sRight
        North -> reverse . sTop
        South -> sBottom
    cellsInSide = selector $ sidesForLevel $ findLevelForCell cell
    offset = length cellsInSide `div` 2
    offsetIndexed = (\(x, i) -> (x, i - offset)) <$> indexed cellsInSide
    cellOffset = snd $ head $ filter (\(x, _) -> x == cell) offsetIndexed

coordForCell :: Int -> (Int, Int)
coordForCell 1 = (0, 0)
coordForCell n = coord
  where
    level = findLevelForCell n
    sides = sidesForCell n
    ilevel = level - 1
    offset = cellOffsetInSide n (head sides)
    coord =
      if length sides == 2
        then case sort sides of
               [East, North] -> (ilevel, ilevel)
               [North, West] -> (-ilevel, ilevel)
               [East, South] -> (ilevel, -ilevel)
               [West, South] -> (-ilevel, -ilevel)
               _ -> error "bah"
        else case head sides of
               East -> (ilevel, offset)
               West -> (-ilevel, offset)
               North -> (offset, ilevel)
               South -> (offset, -ilevel)

isAdjacent :: (Int, Int) -> (Int, Int) -> Bool
isAdjacent (x1, y1) (x2, y2) = nearX && nearY
  where
    isNear a b = abs (a - b) <= 1
    nearX = isNear x1 x2
    nearY = isNear y1 y2

input :: Int
input = 312051

adjacentBeforeCell :: Int -> [Int]
adjacentBeforeCell 1 = []
adjacentBeforeCell n = adjecents
  where
    level = findLevelForCell n
    cells = cellsInLevel (level - 1) ++ takeWhile (< n) (cellsInLevel level)
    adjecents = filter (isAdjacent (coordForCell n) . coordForCell) cells

sumAdjacentBeforeCell :: Int -> Int
sumAdjacentBeforeCell 1 = 1
sumAdjacentBeforeCell n = sum $ map sumAdjacentBeforeCell $ adjacentBeforeCell n

sumAdjacentBeforeCell_ :: (Int -> Int) -> Int -> Int
sumAdjacentBeforeCell_ _ 0 = 0
sumAdjacentBeforeCell_ _ 1 = 1
sumAdjacentBeforeCell_ f n = sum $ map f $ adjacentBeforeCell n

memoize :: (Int -> a) -> (Int -> a)
memoize f = (map f [0 ..] !!)

memoSum :: Int -> Int
memoSum = fix (memoize . sumAdjacentBeforeCell_)

solve :: Int -> Int
solve = memoSum

tests =
  context "stuff" $ do
    describe "coordForCell" $ do
      let expected =
            [ (1, (0, 0))
            , (2, (1, 0))
            , (13, (2, 2))
            , (14, (1, 2))
            , (15, (0, 2))
            , (21, (-2, -2))
            , (22, (-1, -2))
            ]
      it "should work" $
        map (coordForCell . fst) expected `shouldBe` map snd expected
    describe "adjacency" $ do
      it "(0,0) and (1,0) are adjacent" $
        isAdjacent (0, 0) (1, 0) `shouldBe` True
      prop "near in x- and same y are adjacent" $ \x y ->
        (x, y) `isAdjacent` (x - 1, y) `shouldBe` True
      prop "near in x+ and same y are adjacent" $ \x y ->
        (x, y) `isAdjacent` (x + 1, y) `shouldBe` True
      prop "near in x+ and y+ are adjacent" $ \x y ->
        (x, y) `isAdjacent` (x + 1, y + 1) `shouldBe` True
      prop "near in x+ and y- are adjacent" $ \x y ->
        (x, y) `isAdjacent` (x + 1, y - 1) `shouldBe` True
      prop "near in x- and y+ are adjacent" $ \x y ->
        (x, y) `isAdjacent` (x - 1, y + 1) `shouldBe` True
    describe "adjacentBeforeCell" $ do
      let expected =
            [ (1, [])
            , (2, [1])
            , (3, [1, 2])
            , (4, [1, 2, 3])
            , (5, [1, 4])
            , (14, [3, 4, 12, 13])
            , (15, [3, 4, 5, 14])
            , (22, [7, 8, 20, 21])
            ]
      it "should work as expected" $
        map (adjacentBeforeCell . fst) expected `shouldBe` map snd expected
    describe "solution" $ do
      let expected =
            [ (1, 1)
            , (2, 1)
            , (3, 2)
            , (4, 4)
            , (5, 5)
            , (6, 10)
            , (7, 11)
            , (8, 23)
            , (9, 25)
            , (10, 26)
            , (11, 54)
            , (12, 57)
            , (13, 59)
            , (14, 122)
            , (15, 133)
            , (16, 142)
            , (17, 147)
            ]
      it "should work as expected" $
        map (solve . fst) expected `shouldBe` map snd expected

main :: IO ()
main = do
  hspec tests
  let solution = head $ filter (> input) $ map solve [1 ..]
  putStrLn $ "solution: " ++ show solution
