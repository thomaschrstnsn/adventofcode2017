#!/usr/bin/env stack
{- stack
  script
  --resolver lts-9.0
-}
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
size n = dimension n ^ 2

findLevelForCell :: Int -> Int
findLevelForCell c = head $ filter (\d -> size d >= c) [1 ..]

cellsInLevel :: Int -> [Int]
cellsInLevel 1 = [1]
cellsInLevel n = [size (n - 1) + 1 .. size n]

data Sides = Sides
  { sLeft :: [Int]
  , sRight :: [Int]
  , sTop :: [Int]
  , sBottom :: [Int]
  } deriving (Show)

sidesForLevel :: Int -> Sides
sidesForLevel level =
  Sides {sLeft = left, sRight = right, sBottom = bottom, sTop = top}
  where
    cells = cellsInLevel level
    dim = dimension level
    right = take dim $ last cells : cells
    cellsForNextSide lastSide = take dim $ dropWhile (last lastSide /=) cells
    top = cellsForNextSide right
    left = cellsForNextSide top
    bottom = cellsForNextSide left

indexed :: (Enum b, Num b) => [a] -> [(a, b)]
indexed xs = zip xs [0 ..]

cellOffsetInSide :: Eq t => t -> [t] -> Int
cellOffsetInSide cell side = cellOffset
  where
    offset = length side `div` 2
    offsetIndexed = (\(x, i) -> (x, i - offset)) <$> indexed side
    cellOffset = snd $ head $ filter (\(x, _) -> x == cell) offsetIndexed

coordForCell :: Int -> (Int, Int)
coordForCell 1 = (0, 0)
coordForCell n = coord
  where
    level = findLevelForCell n
    sides = sidesForLevel level
    cellIsInSide side = n `elem` side
    rtlb = [sRight, sTop, sLeft, sBottom]
    [right, top, left, bottom] = map (\f -> f sides) rtlb
    sidesForCell = filter cellIsInSide $ map (\f -> f sides) rtlb
    cellSide = head sidesForCell
    ilevel = level - 1
    offset = cellOffsetInSide n cellSide
    coord =
      if length sidesForCell == 2
        then (ilevel, ilevel)
        else case (cellSide ==) of
               right -> (ilevel, offset)
               left -> (-ilevel, offset)
               top -> (offset, ilevel)
               bottom -> (offset, -ilevel)

manhattanDistance :: Num a => (a, a) -> a
manhattanDistance (x, y) = abs x + abs y

input :: Int
input = 312051

solve :: Int -> Int
solve = manhattanDistance . coordForCell

main :: IO ()
main = print $ solve input
