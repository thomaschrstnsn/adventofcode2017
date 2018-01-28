#!/usr/bin/env stack
{- stack
  script
  --resolver lts-9.0
  --package hspec
  --package containers
-}
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes, fromMaybe)
import Prelude hiding (EQ, GT, LT)
import Test.Hspec (describe, hspec, it, shouldBe)

type Register = String

data Operation
  = Increase Int
  | Decrease Int
  deriving (Show, Eq)

data Comparison
  = GT
  | LT
  | EQ
  | NEQ
  | GTEQ
  | LTEQ
  deriving (Show, Eq)

data Condition = Condition
  { cTarget :: Register
  , comparison :: Comparison
  , value :: Int
  } deriving (Show, Eq)

data Instruction = Instruction
  { iTarget :: Register
  , operation :: Operation
  , condition :: Condition
  } deriving (Show, Eq)

-- "inc" "5"
readOperation :: String -> String -> Maybe Operation
readOperation sOp sVal =
  let val = read sVal :: Int
  in case sOp of
       "inc" -> Just $ Increase val
       "dec" -> Just $ Decrease val
       _ -> Nothing

-- "a" "<" "5"
readCondition :: String -> String -> String -> Maybe Condition
readCondition sTarget sComp sVal = do
  let val = read sVal :: Int
  comp <-
    case sComp of
      "<" -> Just LT
      ">" -> Just GT
      "==" -> Just EQ
      ">=" -> Just GTEQ
      "<=" -> Just LTEQ
      "!=" -> Just NEQ
      _ -> Nothing
  return Condition {cTarget = sTarget, comparison = comp, value = val}

--  "b inc 5 if a > 1"
readSingleInstruction :: String -> Maybe Instruction
readSingleInstruction s = do
  let ws = words s
  if length ws == 7 && ws !! 3 == "if"
    then do
      let target = head ws
      op <- readOperation (ws !! 1) (ws !! 2)
      cond <- readCondition (ws !! 4) (ws !! 5) (ws !! 6)
      return Instruction {iTarget = target, operation = op, condition = cond}
    else Nothing

readInstructions :: [String] -> [Instruction]
readInstructions xs = catMaybes $ readSingleInstruction <$> xs

instructionsFromFile :: String -> IO [Instruction]
instructionsFromFile fn = readInstructions . lines <$> readFile fn

type State = Map.Map Register Int

getRegister :: State -> Register -> Int
getRegister s r = Map.findWithDefault 0 r s

evalOperation :: Register -> Operation -> State -> State
evalOperation r op s = Map.alter eval r s
  where
    eval mv =
      Just $
      case op of
        Increase x -> v + x
        Decrease x -> v - x
      where
        v = fromMaybe 0 mv

evalComparison :: Int -> Comparison -> Int -> Bool
evalComparison x comp y = x `eval` y
  where
    eval =
      case comp of
        LT -> (<)
        GT -> (>)
        EQ -> (==)
        NEQ -> (/=)
        LTEQ -> (<=)
        GTEQ -> (>=)

evalCondition :: Condition -> State -> Bool
evalCondition Condition {cTarget = target, comparison = comp, value = val} s =
  evalComparison (getRegister s target) comp val

runSingleInstruction :: State -> Instruction -> State
runSingleInstruction s i =
  if conditionPassed
    then evalOperation (iTarget i) (operation i) s
    else s
  where
    conditionPassed = evalCondition (condition i) s

runInstructions :: [Instruction] -> State -> State
runInstructions is s = foldl runSingleInstruction s is

switch :: (a, b) -> (b, a)
switch (x, y) = (y, x)

findMax :: State -> (Register, Int)
findMax s = switch $ Map.findMax $ Map.fromList $ switch <$> Map.toList s

solve :: [Instruction] -> (Register, Int)
solve is = findMax $ runInstructions is Map.empty

example :: [Instruction]
example =
  readInstructions
    [ "b inc 5 if a > 1"
    , "a inc 1 if b < 5"
    , "c dec -10 if a >= 1"
    , "c inc -20 if c == 10"
    ]

input :: IO [Instruction]
input = instructionsFromFile "day08input.txt"

tests = do
  describe "readInstructions" $ do
    it "works" $
      readInstructions ["b inc 5 if a > 1"] `shouldBe`
      [ Instruction
        { iTarget = "b"
        , operation = Increase 5
        , condition = Condition {cTarget = "a", comparison = GT, value = 1}
        }
      ]
    it "reads example" $ length example `shouldBe` 4
    it "reads input" $ do
      inp <- input
      length inp `shouldBe` 1000
  describe "evaluation" $ do
    describe "evalComparison" $ do
      it "LT" $ evalComparison 0 LT 5 `shouldBe` True
      it "EQ" $ evalComparison 5 EQ 6 `shouldBe` False
    describe "runInstructions" $
      it "example" $
      runInstructions example Map.empty `shouldBe`
      Map.fromList [("a", 1), ("c", -10)]
    describe "runSingleInstruction" $
      it "first from example" $
      runSingleInstruction Map.empty (head example) `shouldBe` Map.empty
  describe "solve" $ it "works with example" $ solve example `shouldBe` ("a", 1)

main :: IO ()
main = do
  print example
  hspec tests
  inp <- input
  putStrLn $ "solution: " ++ show (solve inp)
