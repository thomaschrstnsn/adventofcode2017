#!/usr/bin/env stack
{- stack
  script
  --resolver lts-9.0
  --package hspec
  --package hspec-core
  --package parsec
  --package vector
  --package containers
-}
{-# LANGUAGE RecordWildCards #-}

import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import qualified Data.Vector as V
import Data.Vector (Vector, (!?))
import Test.Hspec (SpecWith, describe, hspec, it, shouldBe)
import Text.ParserCombinators.Parsec hiding (Parser, State)

type Register = Char

data Operand
  = OLit Int
  | OReg Register
  deriving (Show, Eq)

type Pair = (Register, Operand)

data Instruction
  = Send Operand
  | Set Pair
  | Add Pair
  | Mult Pair
  | Mod Pair
  | Receive Register
  | JumpGreaterThanZero (Operand, Operand)
  deriving (Show, Eq)

type Parser a = GenParser Char () a

parser :: Parser [Instruction]
parser = manyTill (pInstruction <* newline) eof

pInstruction :: Parser Instruction
pInstruction =
  choice
    [ pInstOp "snd" Send
    , pInstReg "rcv" Receive
    , pInstPair "set" Set
    , pInstPair "add" Add
    , pInstPair "mul" Mult
    , pInstPair "mod" Mod
    , pInstOpOp "jgz" JumpGreaterThanZero
    ]

pInstOp :: String -> (Operand -> Instruction) -> Parser Instruction
pInstOp s ctor = do
  _ <- try $ string s
  _ <- space
  ctor <$> pOperand

pInstReg :: String -> (Register -> Instruction) -> Parser Instruction
pInstReg s ctor = do
  _ <- try $ string s
  _ <- space
  ctor <$> letter

pInstPair :: String -> (Pair -> Instruction) -> Parser Instruction
pInstPair s ctor = do
  _ <- try $ string s
  _ <- space
  r <- letter
  _ <- space
  o <- pOperand
  return $ ctor (r, o)

pInstOpOp :: String -> ((Operand, Operand) -> Instruction) -> Parser Instruction
pInstOpOp s ctor = do
  _ <- try $ string s
  _ <- space
  o1 <- pOperand
  _ <- space
  o2 <- pOperand
  return $ ctor (o1, o2)

pOperand :: Parser Operand
pOperand = pLit <|> pReg

readNeg :: String -> Int
readNeg ('-':ds) = negate $ read ds
readNeg x = read x

negNumber :: Parser String
negNumber = do
  first <- char '-'
  rest <- many1 digit
  return $ first : rest

pInt :: Parser Int
pInt = readNeg <$> (many1 digit <|> negNumber)

pLit :: Parser Operand
pLit = OLit <$> pInt

pReg :: Parser Operand
pReg = OReg <$> letter

readInstructions :: String -> Either String [Instruction]
readInstructions x = do
  let noBlanks = unlines $ filter (/= "") $ lines x
  let res = parse parser "in" noBlanks
  case res of
    (Left err) -> Left $ show err
    (Right r) -> Right r

data ProgramState = ProgramState
  { registers :: Map Register Int
  , instructionPointer :: Int
  , incoming :: [Int]
  , outgoing :: [Int]
  , totalSends :: Int
  , psId :: Int
  , blocked :: Bool
  } deriving (Show, Eq)

data State = State
  { running :: ProgramState
  , other :: ProgramState
  } deriving (Show, Eq)

initialProgram :: Int -> ProgramState
initialProgram i =
  ProgramState
  { registers = Map.singleton 'p' i
  , instructionPointer = 0
  , incoming = []
  , outgoing = []
  , totalSends = 0
  , psId = i
  , blocked = False
  }

initial :: State
initial = State {running = initialProgram 0, other = initialProgram 1}

data StopReason
  = JumpOutside
  | EmptyReceive

push :: [a] -> a -> [a]
push xs x = xs ++ [x]

pop :: [a] -> Maybe (a, [a])
pop [] = Nothing
pop (x:xs) = Just (x, xs)

eval :: Vector Instruction -> ProgramState -> (ProgramState, StopReason)
eval instructions state@ProgramState {..} =
  case instructions !? instructionPointer of
    Nothing -> (state, JumpOutside)
    Just i ->
      let valOf operand =
            case operand of
              OReg a -> Map.findWithDefault 0 a registers
              OLit x -> x
          update' register f s =
            step $ s {registers = Map.alter f register registers}
          update register f = update' register f state
          step s =
            eval instructions s {instructionPointer = instructionPointer + 1}
          jump = eval instructions
      in case i of
           Set (r, o) -> update r $ const $ Just $ valOf o
           Add (r, o) -> update r (fmap (+ valOf o))
           Mult (r, o) -> update r (fmap (* valOf o))
           Mod (r, o) -> update r (fmap (`mod` valOf o))
           JumpGreaterThanZero (o1, o2) ->
             if valOf o1 > 0
               then jump $
                    state {instructionPointer = instructionPointer + valOf o2}
               else step state
           Send o ->
             step $
             state
             {outgoing = push outgoing $ valOf o, totalSends = totalSends + 1}
           Receive r ->
             case pop incoming of
               Just (received, nextIncoming) ->
                 update' r (const $ Just received) $
                 state {blocked = False, incoming = nextIncoming}
               Nothing -> (state {blocked = True}, EmptyReceive)

solve' :: Vector Instruction -> State -> State
solve' instructions s@State {..} =
  case eval instructions running of
    (s', JumpOutside) -> s {running = s'}
    (s', EmptyReceive) ->
      let sent = outgoing s'
      in if blocked other && null sent
           then s {running = s'}
           else let otherIncoming = incoming other ++ sent
                in solve' instructions $
                   s
                   { running = other {incoming = otherIncoming}
                   , other = s' {outgoing = []}
                   }

solve :: [Instruction] -> Int
solve ins = totalSends program1
  where
    resultState = solve' (V.fromList ins) initial
    program1 =
      head $ filter ((== 1) . psId) [other resultState, running resultState]

readAndSolve :: String -> Either String Int
readAndSolve s = solve <$> readInstructions s

input :: IO String
input = readFile "day18input.txt"

tests :: SpecWith ()
tests =
  describe "parser" $ do
    it "for single" $
      readInstructions "set a 1" `shouldBe` Right [Set ('a', OLit 1)]
    it "for example" $
      readInstructions
        (unlines
           [ "set a 1"
           , "add a 2"
           , "mul a a"
           , "mod a 5"
           , "snd a"
           , "set a 0"
           , "rcv a"
           , "jgz a -1"
           , "set a 1"
           , "jgz a -2"
           ]) `shouldBe`
      Right
        [ Set ('a', OLit 1)
        , Add ('a', OLit 2)
        , Mult ('a', OReg 'a')
        , Mod ('a', OLit 5)
        , Send (OReg 'a')
        , Set ('a', OLit 0)
        , Receive 'a'
        , JumpGreaterThanZero (OReg 'a', OLit (-1))
        , Set ('a', OLit 1)
        , JumpGreaterThanZero (OReg 'a', OLit (-2))
        ]

main :: IO ()
main = do
  hspec tests
  inp <- input
  putStrLn $ "solution: " ++ show (readAndSolve inp)
