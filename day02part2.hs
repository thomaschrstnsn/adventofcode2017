#!/usr/bin/env stack
{- stack
  script
  --resolver lts-9.0
-}
import Data.List (tails)

example :: [String]
example = ["5 9 2 8", "9 4 7 3", "3 8 6 5"]

expected :: Integer
expected = 9

input :: [String]
input =
  [ "1136 1129 184 452 788 1215 355 1109 224 1358 1278 176 1302 186 128 1148"
  , "242 53 252 62 40 55 265 283 38 157 259 226 322 48 324 299"
  , "2330 448 268 2703 1695 2010 3930 3923 179 3607 217 3632 1252 231 286 3689"
  , "89 92 903 156 924 364 80 992 599 998 751 827 110 969 979 734"
  , "100 304 797 81 249 1050 90 127 675 1038 154 715 79 1116 723 990"
  , "1377 353 3635 99 118 1030 3186 3385 1921 2821 492 3082 2295 139 125 2819"
  , "3102 213 2462 116 701 2985 265 165 248 680 3147 1362 1026 1447 106 2769"
  , "5294 295 6266 3966 2549 701 2581 6418 5617 292 5835 209 2109 3211 241 5753"
  , "158 955 995 51 89 875 38 793 969 63 440 202 245 58 965 74"
  , "62 47 1268 553 45 60 650 1247 1140 776 1286 200 604 399 42 572"
  , "267 395 171 261 79 66 428 371 257 284 65 25 374 70 389 51"
  , "3162 3236 1598 4680 2258 563 1389 3313 501 230 195 4107 224 225 4242 4581"
  , "807 918 51 1055 732 518 826 806 58 394 632 36 53 119 667 60"
  , "839 253 1680 108 349 1603 1724 172 140 167 181 38 1758 1577 748 1011"
  , "1165 1251 702 282 1178 834 211 1298 382 1339 67 914 1273 76 81 71"
  , "6151 5857 4865 437 6210 237 37 410 544 214 233 6532 2114 207 5643 6852"
  ]

pairs :: [a] -> [(a, a)]
pairs xs = [(x, y) | (x:ys) <- tails xs, y <- ys]

pairsBothWays :: [t] -> [(t, t)]
pairsBothWays xs = concatMap (\(x, y) -> [(x, y), (y, x)]) $ pairs xs

rowSum :: [Integer] -> Integer
rowSum xs = d1 `div` d2
  where
    divisors = filter (\(x, y) -> x `rem` y == 0) $ pairsBothWays xs
    (d1, d2) = head divisors

checksum :: [[Integer]] -> Integer
checksum xss = sum $ fmap rowSum xss

parse :: [String] -> [[Integer]]
parse = fmap lineToIntegers
  where
    lineToIntegers s = (\x -> read x :: Integer) <$> words s

main :: IO ()
main = do
  print $ checksum $ parse example
  print $ checksum $ parse input
