#!/usr/bin/env stack
{- stack
  script
  --resolver lts-9.0
  --package hspec
  --package hspec-core
  --package containers
  --package primitive
  --package vector
-}
import Control.Monad (when)
import Control.Monad.Primitive (PrimMonad, PrimState)
import Control.Monad.ST (runST)
import Data.Bits (testBit)
import Data.Maybe (catMaybes, fromJust, isJust)
import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Vector.Mutable (MVector)
import qualified Data.Vector.Mutable as MV
import KnotHash (knotHash)
import Specs (specFromExamples, specItem)
import Test.Hspec (SpecWith, describe, hspec, it, shouldBe)

data Square
  = Free
  | Used
  deriving (Show, Eq)

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

data Region
  = Region Int
  | Blank
  deriving (Show, Eq)

data RegionInTheMaking
  = R Region
  | S Square
  deriving (Eq)

instance Show RegionInTheMaking where
  show (R (Region i)) = show i
  show (R Blank) = " "
  show (S Used) = "x"
  show (S Free) = "."

mkRegionInTheMakings :: VVector Square -> VVector RegionInTheMaking
mkRegionInTheMakings squares = (\row -> S <$> row) <$> squares

isUsedSquare :: RegionInTheMaking -> Bool
isUsedSquare (S Used) = True
isUsedSquare _ = False

type VVector a = Vector (Vector a)

type MVVector m a = MVector m (MVector m a)

vvThaw :: PrimMonad m => VVector a -> m (MVVector (PrimState m) a)
vvThaw vss = do
  mvs <- mapM V.thaw vss
  V.thaw mvs

vvFreeze :: PrimMonad m => MVVector (PrimState m) a -> m (VVector a)
vvFreeze mvs = do
  frozen <- mapM (freezeByIndex mvs) [0 .. (MV.length mvs - 1)]
  return $ V.fromList frozen
  where
    freezeByIndex v' i = do
      x <- MV.read v' i
      V.freeze x

fromListOfLists :: [[a]] -> VVector a
fromListOfLists xss = V.fromList $ V.fromList <$> xss

validCoordinate :: (Int, Int) -> Bool
validCoordinate (x, y) = validPart x && validPart y
  where
    validPart p = p >= 0 && p < 128

floodFill ::
     PrimMonad m
  => (Int, Int)
  -> Int
  -> MVVector (PrimState m) RegionInTheMaking
  -> m ()
floodFill (x, y) regionNumber rs = do
  row <- MV.read rs x
  node <- MV.read row y
  when (isUsedSquare node) $
    sequence_
      [ MV.write row y (R $ Region regionNumber)
      , tryDirection (1, 0) rs
      , tryDirection (-1, 0) rs
      , tryDirection (0, 1) rs
      , tryDirection (0, -1) rs
      ]
  where
    tryDirection ::
         (PrimMonad m)
      => (Int, Int)
      -> MVVector (PrimState m) RegionInTheMaking
      -> m ()
    tryDirection (dx, dy) regions =
      when (validCoordinate coord) $ floodFill coord regionNumber regions
      where
        coord = (x + dx, y + dy)

makeRegions :: (PrimMonad m) => VVector Square -> m (VVector Region)
makeRegions squares = do
  r <- makeRegions' $ mkRegionInTheMakings squares
  return $ final r
  where
    regionOrBust (R r) = r
    regionOrBust (S Free) = Blank
    regionOrBust _ = error "not everything was made into regions"
    final :: VVector RegionInTheMaking -> VVector Region
    final inp = (\row -> regionOrBust <$> row) <$> inp

makeRegions' ::
     (PrimMonad m) => VVector RegionInTheMaking -> m (VVector RegionInTheMaking)
makeRegions' regions' = do
  regions <- vvThaw regions'
  _ <- makeRegions'' regions 1
  vvFreeze regions

tryHead :: [a] -> Maybe a
tryHead [] = Nothing
tryHead (x:_) = Just x

firstIndexInMVector ::
     PrimMonad m => (a -> Bool) -> MVector (PrimState m) a -> m (Maybe Int)
firstIndexInMVector f v = do
  maybes <- mapM (findByIndex v) [0 .. (MV.length v - 1)]
  return $ tryHead $ catMaybes maybes
  where
    findByIndex v' i = do
      x <- MV.read v' i
      if f x
        then return $ Just i
        else return Nothing

firstIndexInMVectorM ::
     PrimMonad m => (a -> m Bool) -> MVector (PrimState m) a -> m (Maybe Int)
firstIndexInMVectorM f v = do
  maybes <- mapM (findByIndex v) [0 .. (MV.length v - 1)]
  return $ tryHead $ catMaybes maybes
  where
    findByIndex v' i = do
      x <- MV.read v' i
      b <- f x
      if b
        then return $ Just i
        else return Nothing

findUsedSquareInRow ::
     PrimMonad m => MVector (PrimState m) RegionInTheMaking -> m (Maybe Int)
findUsedSquareInRow = firstIndexInMVector isUsedSquare

findUsedSquare ::
     PrimMonad m
  => MVVector (PrimState m) RegionInTheMaking
  -> m (Maybe (Int, Int))
findUsedSquare rows = do
  rowIndex <- firstIndexInMVectorM rowHasUsedSquare rows
  case rowIndex of
    Just ri -> do
      row <- MV.read rows ri
      maybeColumnIndex <- findUsedSquareInRow row
      return $ Just (ri, fromJust maybeColumnIndex)
    Nothing -> return Nothing
  where
    rowHasUsedSquare r = do
      maybeIndex <- findUsedSquareInRow r
      return $ isJust maybeIndex

makeRegions'' ::
     PrimMonad m => MVVector (PrimState m) RegionInTheMaking -> Int -> m ()
makeRegions'' regions regionNumber = do
  maybeCoord <- findUsedSquare regions
  case maybeCoord of
    Just coord -> do
      _ <- floodFill coord regionNumber regions
      makeRegions'' regions (regionNumber + 1)
    Nothing -> return ()

countRegions :: [[Square]] -> Int
countRegions squares =
  findBiggest $ runST (makeRegions $ fromListOfLists squares)
  where
    regionAsInt :: Region -> Int
    regionAsInt (Region i) = i
    regionAsInt Blank = 0
    unfold :: VVector a -> Vector a
    unfold xs = V.concat $ V.toList xs
    findBiggest :: VVector Region -> Int
    findBiggest regions = maximum $ regionAsInt <$> unfold regions

solve :: String -> Int
solve s = countRegions $ squaresForHash s

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

main :: IO ()
main = do
  hspec tests
  putStrLn $ "solution: " ++ show (solve input)
