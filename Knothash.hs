{-# LANGUAGE ViewPatterns #-}

module KnotHash
  ( asciiToInt
  , rotate
  , hash
  , sparseToDense
  , hexadecimal
  , knotHash
  ) where

import Data.Bits (xor)
import Data.Char (ord)
import Data.Foldable (toList)
import qualified Data.Sequence as Seq
import Data.Sequence (Seq, ViewL(..), (><))
import Numeric (showHex)

asciiToInt :: String -> Seq Int
asciiToInt s = ord <$> Seq.fromList s

rotate :: Seq Int -> Int -> Int -> Seq Int
rotate xs index' len = result
  where
    l = Seq.length xs
    index = index' `mod` l
    ls = xs >< xs
    reversed = Seq.reverse $ Seq.take len $ Seq.drop index ls
    spliced = Seq.drop (len + index) ls
    total = Seq.take l (reversed >< spliced)
    (t, h) = Seq.splitAt (l - index) total
    result = h >< t

hash :: Int -> Seq Int -> Seq Int -> Seq Int
hash rounds circlist lengths = go 0 0 circlist lengths (rounds - 1)
  where
    go :: Int -> Int -> Seq Int -> Seq Int -> Int -> Seq Int
    go _ _ cl (Seq.viewl -> EmptyL) 0 = cl
    go index skipsize cl (Seq.viewl -> EmptyL) r =
      go index skipsize cl lengths (r - 1)
    go index skipsize cl (Seq.viewl -> l :< ls) r =
      go (index + skipsize + l) (1 + skipsize) (rotate cl index l) ls r
    go _ _ _ _ _ = error "shut up warning"

sparseToDense :: [Int] -> [Int]
sparseToDense [] = []
sparseToDense xs = xored : sparseToDense rest
  where
    (these, rest) = splitAt 16 xs
    xored = foldl xor 0 these

hexadecimal :: [Int] -> String
hexadecimal = foldMap toHex
  where
    toHex i =
      let hex = showHex i ""
      in if length hex == 1
           then '0' : hex
           else hex

extraLength :: Seq Int
extraLength = Seq.fromList [17, 31, 73, 47, 23]

knotHash :: String -> String
knotHash s =
  hexadecimal $
  sparseToDense $ toList $ hash 64 (Seq.fromList [0 .. 255]) (il >< extraLength)
  where
    il = asciiToInt s
