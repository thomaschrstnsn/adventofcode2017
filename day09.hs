#!/usr/bin/env stack
{- stack
  script
  --resolver lts-9.0
  --package hspec
  --package hspec-core
  --package parsec
-}
import Data.Monoid (Sum(..), getSum)
import Test.Hspec (SpecWith, describe, hspec, it, shouldBe)
import Test.Hspec.Core.Spec (fromSpecList, specItem)
import Text.ParserCombinators.Parsec

data Stream
  = Group [Stream]
  | Garbage String
  deriving (Show, Eq)

type StreamParser = GenParser Char () Stream

parser :: StreamParser
parser = do
  result <- pStream
  _ <- many newline
  eof
  return result

pStream :: StreamParser
pStream = pGroup <|> pGarbage

pGroup :: StreamParser
pGroup = Group <$> between (char '{') (char '}') (sepBy pStream (char ','))

pGarbage :: StreamParser
pGarbage = Garbage <$> between (char '<') (char '>') inside
  where
    escaped = do
      _ <- char '!'
      anyChar
    inside = many (escaped <|> noneOf ">")

readStream :: String -> Either String Stream
readStream x = do
  let res = parse parser "in" x
  case res of
    (Left err) -> Left $ show err
    (Right r) -> Right r

score :: Stream -> Int
score = go 1
  where
    go :: Int -> Stream -> Int
    go l (Group cs) = l + getSum (foldMap Sum $ map (go (l + 1)) cs)
    go _ (Garbage _) = 0

solve :: String -> Either String Int
solve s = do
  stream <- readStream s
  return $ score stream

input :: IO String
input = readFile "day09input.txt"

specFromExamples examples builder = fromSpecList $ map builder examples

garbageSpec =
  specFromExamples
    [ ("<>", "")
    , ("<random characters>", "random characters")
    , ("<<<<>", "<<<")
    , ("<{!>}>", "{>}")
    , ("<" ++ "!!" ++ ">", "!")
    , ("<" ++ "!!!>" ++ ">", "!>")
    , ("<{o\"i!a,<{i<a>", "{o\"ia,<{i<a")
    ]
    (\(input, expected) ->
       specItem ("'" ++ input ++ "' reads as expected") $
       readStream input `shouldBe` (Right $ Garbage expected))

groupSpec =
  specFromExamples
    [ ("{}", g [])
    , ("{{{}}}", g [g [eg]])
    , ("{{},{}}", g [eg, eg])
    , ("{{{},{},{{}}}}", g [g [eg, eg, g [eg]]])
    , ("{<a>,<a>,<a>,<a>}", g [ga, ga, ga, ga])
    , ("{{<!>},{<!>},{<!>},{<a>}}", g [g [Garbage ">},{<>},{<>},{<a"]])
    ]
    (\(input, expected) ->
       specItem ("'" ++ input ++ "' reads as expected") $
       readStream input `shouldBe` Right expected)
  where
    g = Group
    eg = Group []
    ga = Garbage "a"

scoreSpec =
  specFromExamples
    [ ("{}", 1)
    , ("{{{}}}", 6)
    , ("{{},{}}", 5)
    , ("{{{},{},{{}}}}", 16)
    , ("{<a>,<a>,<a>,<a>}", 1)
    , ("{{<ab>},{<ab>},{<ab>},{<ab>}}", 9)
    , ("{{<!!>},{<!!>},{<!!>},{<!!>}}", 9)
    , ("{{<a!>},{<a!>},{<a!>},{<ab>}}", 3)
    ]
    (\(inp, expected) ->
       specItem ("'" ++ inp ++ "' should score: " ++ show expected) $
       score <$> readStream inp `shouldBe` Right expected)

tests = do
  describe "readStream" $ do
    describe "garbage examples" garbageSpec
    describe "group examples" groupSpec
  describe "score" scoreSpec

main :: IO ()
main = do
  hspec tests
  inp <- input
  putStrLn $ "solution: " ++ show (solve inp)
