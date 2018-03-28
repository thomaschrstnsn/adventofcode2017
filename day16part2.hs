#!/usr/bin/env stack
{- stack
  script
  --resolver lts-9.0
  --package hspec
  --package hspec-core
  --package parsec
  --package vector
  --package primitive
-}
import Control.Monad (when)
import Control.Monad.Primitive (PrimMonad, PrimState)
import Control.Monad.ST (runST)
import qualified Data.Vector as V
import Data.Vector.Mutable (MVector)
import qualified Data.Vector.Mutable as MV
import Test.Hspec (SpecWith, describe, hspec, it, shouldBe)
import Text.ParserCombinators.Parsec hiding (Parser)

data Move
  = Spin Int
  | Exchange (Int, Int)
  | Partner (Char, Char)
  deriving (Show, Eq)

type Program = Char

type Parser a = GenParser Char () a

parser :: Parser [Move]
parser = do
  result <- sepBy pMove (char ',')
  eof
  return result

pMove :: Parser Move
pMove = pSpin <|> pExchange <|> pPartner

pInt :: Parser Int
pInt = readInt <$> many1 digit
  where
    readInt :: String -> Int
    readInt = read

pSpin :: Parser Move
pSpin = do
  _ <- char 's'
  Spin <$> pInt

pExchange :: Parser Move
pExchange = do
  _ <- char 'x'
  i <- pInt
  _ <- char '/'
  j <- pInt
  return $ Exchange (i, j)

pPartner :: Parser Move
pPartner = do
  _ <- char 'p'
  a <- letter
  _ <- char '/'
  b <- letter
  return $ Partner (a, b)

readMoves :: String -> Either String [Move]
readMoves x = do
  let noNewLines = mconcat $ lines x
  let res = parse parser "in" noNewLines
  case res of
    (Left err) -> Left $ show err
    (Right r) -> Right r

solve :: [Program] -> [Move] -> Int -> [Program]
solve programList moves repeats =
  runST $ toVectorAndBack programList (\v -> solveMV v moves repeats)

spin :: PrimMonad m => MVector (PrimState m) Program -> Int -> m ()
spin v n = do
  f <- V.freeze v
  let l = rotate n (V.toList f)
  v' <- V.thaw $ V.fromList l
  _ <- MV.copy v v'
  return ()

rotate :: Int -> [a] -> [a]
rotate n xs = reverse (bs ++ as)
  where
    (as, bs) = splitAt n (reverse xs)

exchange :: PrimMonad m => MVector (PrimState m) Program -> Int -> Int -> m ()
exchange v i j = do
  vI <- MV.read v i
  vJ <- MV.read v j
  _ <- MV.write v i vJ
  _ <- MV.write v j vI
  return ()

partner :: PrimMonad m => MVector (PrimState m) Program -> Char -> Char -> m ()
partner v a b = mapM_ change [0 .. (MV.length v - 1)]
  where
    change i = do
      x <- MV.read v i
      if x == a
        then MV.write v i b
        else when (x == b) $ MV.write v i a

solveMV :: PrimMonad m => MVector (PrimState m) Program -> [Move] -> Int -> m ()
solveMV programs moves n = mapM_ (\_ -> solveMV' programs moves) [1 .. n]

solveMV' :: PrimMonad m => MVector (PrimState m) Program -> [Move] -> m ()
solveMV' _ [] = return ()
solveMV' programs (m:ms) = do
  _ <-
    case m of
      Spin x -> spin programs x
      Exchange (i, j) -> exchange programs i j
      Partner (a, b) -> partner programs a b
  solveMV' programs ms

findCycle :: [Program] -> [Move] -> Either String Int
findCycle ps ms =
  case pss of
    [] -> Left "Could not find cycle"
    (_, x):_ -> Right x
  where
    pss =
      filter ((ps ==) . fst) $
      take 4650 $ zip (drop 1 $ iterate (\ps' -> solve ps' ms 1) ps) [1 ..]

readAndSolve :: String -> [Program] -> Int -> Either String [Program]
readAndSolve s ps n = do
  moves <- readMoves s
  cycleIteration <- findCycle ps moves
  let iterationsToRun = n `mod` cycleIteration
  return $ solve ps moves iterationsToRun

input :: IO String
input = readFile "day16input.txt"

toVectorAndBack ::
     PrimMonad m => [a] -> (MVector (PrimState m) a -> m ()) -> m [a]
toVectorAndBack xs op = do
  v <- V.thaw $ V.fromList xs
  _ <- op v
  fv <- V.freeze v
  return $ V.toList fv

tests :: SpecWith ()
tests = do
  describe "Dance moves work as intended" $ do
    it "spin" $ runST (toVectorAndBack "abcde" (`spin` 1)) `shouldBe` "eabcd"
    it "exchange" $
      runST (toVectorAndBack "eabcd" (\p -> exchange p 3 4)) `shouldBe` "eabdc"
    it "partner" $
      runST (toVectorAndBack "eabdc" (\p -> partner p 'e' 'b')) `shouldBe`
      "baedc"
  describe "parsing works" $ do
    it "spin" $ readMoves "s1" `shouldBe` Right [Spin 1]
    it "exchange" $ readMoves "x3/4" `shouldBe` Right [Exchange (3, 4)]
    it "partner" $ readMoves "pe/b" `shouldBe` Right [Partner ('e', 'b')]
    it "multiple" $
      readMoves "x3/11,s10,x0/10" `shouldBe`
      Right [Exchange (3, 11), Spin 10, Exchange (0, 10)]
  describe "Example" $
    it "works as intended" $
    solve "abcde" [Spin 1, Exchange (3, 4), Partner ('e', 'b')] 1 `shouldBe`
    "baedc"

main :: IO ()
main = do
  hspec tests
  inp <- input
  putStrLn $
    "solution: " ++ show (readAndSolve inp ['a' .. 'p'] (1000 * 1000 * 1000))
