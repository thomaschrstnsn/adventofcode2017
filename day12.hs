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
import Data.Set (Set)
import qualified Data.Set as Set
import Test.Hspec (SpecWith, describe, hspec, it, shouldBe)

type Program = Int

type ProgramConnection = (Program, [Program])

readInt :: String -> Either String Int
readInt s =
  if all isDigit s
    then Right (read s)
    else Left "Expected digits"

readSingleProgramConnection :: String -> Either String ProgramConnection
readSingleProgramConnection s =
  case words s of
    (sp1:(sep:rest)) -> do
      p1 <- readInt sp1
      _ <-
        if sep == "<->"
          then Right ()
          else Left "Expected seperator <->"
      connected <-
        case partitionEithers $ (readInt . stripComma) <$> rest of
          ([], ps) -> Right ps
          (xs, _) -> Left $ mconcat xs
      return (p1, connected)
    _ -> Left "Expected atleast 3 words in string"
  where
    stripComma = takeWhile (/= ',')

readProgramConnections :: [String] -> Either [String] [ProgramConnection]
readProgramConnections ls =
  case partitionEithers $ readSingleProgramConnection <$> ls of
    ([], connections) -> Right connections
    (errs, _) -> Left errs

connectedTo :: [ProgramConnection] -> Set Program -> Set Program
connectedTo connections source = foldl helper source connections
  where
    eitherIn :: Set Program -> ProgramConnection -> Set Program
    eitherIn s (p, ps) =
      if or $ fmap (`Set.member` s) $ p : ps
        then Set.fromList $ p : ps
        else Set.empty
    helper :: Set Program -> ProgramConnection -> Set Program
    helper s pc = Set.union s $ eitherIn s pc

converge :: (a -> a -> Bool) -> [a] -> a
converge p (x:ys@(y:_))
  | p x y = y
  | otherwise = converge p ys
converge _ _ = error "oops"

simplify :: Eq a => (a -> a) -> a -> a
simplify f = converge (==) . iterate f

convergingConnectedTo :: [ProgramConnection] -> Set Program -> Set Program
convergingConnectedTo cps = simplify (connectedTo cps)

solve :: [ProgramConnection] -> Int
solve cps = Set.size $ convergingConnectedTo cps (Set.singleton 0)

input :: IO [ProgramConnection]
input = do
  file <- readFile "day12input.txt"
  let eitherPaths = readProgramConnections $ lines file
  return $ either (error . mconcat) id eitherPaths

example :: [String]
example =
  [ "0 <-> 2"
  , "1 <-> 1"
  , "2 <-> 0, 3, 4"
  , "3 <-> 2, 4"
  , "4 <-> 2, 3, 6"
  , "5 <-> 6"
  , "6 <-> 4, 5"
  ]

tests :: SpecWith ()
tests =
  describe "solve" $
  it "works with example" $
  solve <$> readProgramConnections example `shouldBe` Right 6

main :: IO ()
main = do
  inp <- input
  print $ length inp
  hspec tests
  putStrLn $ "solution: " ++ show (solve inp)
