#!/usr/bin/env stack
{- stack
  script
  --resolver lts-9.0
  --package hspec
  --package hspec-core
  --package parsec
-}
import Data.List (minimumBy)
import Test.Hspec (SpecWith, describe, hspec, it, shouldBe)
import Text.ParserCombinators.Parsec hiding (Parser, State)

type Vector = (Int, Int, Int)

data Particle = Particle
  { position :: Vector
  , velocity :: Vector
  , acceleration :: Vector
  } deriving (Show, Eq)

type Parser a = GenParser Char () a

parser :: Parser [Particle]
parser = manyTill (pParticle <* newline) eof

pSep :: Parser ()
pSep = char ',' >> spaces

pParticle :: Parser Particle
pParticle = do
  pos <- pPos
  _ <- pSep
  vel <- pVel
  _ <- pSep
  acc <- pAcc
  return Particle {position = pos, velocity = vel, acceleration = acc}
  where
    pPos = pVector 'p'
    pVel = pVector 'v'
    pAcc = pVector 'a'

pVector :: Char -> Parser Vector
pVector c = do
  _ <- char c
  _ <- char '='
  _ <- char '<'
  _ <- spaces
  x <- pInt
  _ <- pSep
  y <- pInt
  _ <- pSep
  z <- pInt
  _ <- char '>'
  return (x, y, z)

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

readParticles :: String -> Either String [Particle]
readParticles x = do
  let noBlanks = unlines $ filter (/= "") $ lines x
  let res = parse parser "in" noBlanks
  case res of
    (Left err) -> Left $ show err
    (Right r) -> Right r

add :: Vector -> Vector -> Vector
add (x1, y1, z1) (x2, y2, z2) = (x1 + x2, y1 + y2, z1 + z2)

update :: Particle -> Particle
update p = p {velocity = v', position = p'}
  where
    v' = velocity p `add` acceleration p
    p' = position p `add` v'

updateMany :: [Particle] -> [Particle]
updateMany = map update

score :: Particle -> Int
score p = manhattan $ position p

manhattan :: Vector -> Int
manhattan (x, y, z) = abs x + abs y + abs z

compBy :: Ord b => (a -> b) -> (a -> a -> Ordering)
compBy f a b = compare (f a) (f b)

scoreMany :: [Particle] -> Int
scoreMany ps = snd $ minimumBy (compBy (score . fst)) $ zip ps [0 :: Int ..]

solve :: [Particle] -> Int
solve ps = scoreMany $ head $ drop 1000 $ iterate updateMany ps

readAndSolve :: String -> Either String Int
readAndSolve s = solve <$> readParticles s

input :: IO String
input = readFile "day20input.txt"

tests :: SpecWith ()
tests =
  describe "parser" $ do
    it "for first example" $
      readParticles "p=< 3,0,0>, v=< 2,0,0>, a=<-1,0,0>" `shouldBe`
      Right
        [ Particle
          { position = (3, 0, 0)
          , velocity = (2, 0, 0)
          , acceleration = (-1, 0, 0)
          }
        ]
    it "for second example" $
      readParticles "p=<1,2,3>, v=<4,5,6>, a=< -7,8,-9>" `shouldBe`
      Right
        [ Particle
          { position = (1, 2, 3)
          , velocity = (4, 5, 6)
          , acceleration = (-7, 8, -9)
          }
        ]

main :: IO ()
main = do
  hspec tests
  inp <- input
  putStrLn $ "solution: " ++ show (readAndSolve inp)
