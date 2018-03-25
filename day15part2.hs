#!/usr/bin/env stack
{- stack
  script
  --resolver lts-9.0
  --package hspec
  --package hspec-core
-}
import Data.Bits ((.&.))
import Test.Hspec (SpecWith, describe, hspec, it, shouldBe)

first16bitsMatch :: Int -> Int -> Bool
first16bitsMatch a b = da == db
  where
    divide x = x .&. 0xFFFF
    da = divide a
    db = divide b

multipleOf :: Int -> Int -> Bool
multipleOf m x = x `rem` m == 0

solve :: (Int, Int) -> Int
solve (as, bs) =
  length $
  filter (uncurry first16bitsMatch) $
  take (5 * 1000 * 1000) $
  zip (filter (multipleOf 4) $ genA as) (filter (multipleOf 8) $ genB bs)

genX :: Int -> Int -> [Int]
genX mult start = drop 1 $ iterate (\x -> (x * mult) `rem` 2147483647) start

genA :: Int -> [Int]
genA = genX 16807

genB :: Int -> [Int]
genB = genX 48271

input :: (Int, Int)
input = (699, 124)

example :: (Int, Int)
example = (65, 8921)

tests :: SpecWith ()
tests = do
  describe "generators" $ do
    it "A's example" $
      take 5 (genA $ fst example) `shouldBe`
      [1092455, 1181022009, 245556042, 1744312007, 1352636452]
    it "B's example" $
      take 5 (genB $ snd example) `shouldBe`
      [430625591, 1233683848, 1431495498, 137874439, 285222916]
  describe "first16bitsMatch" $ do
    it "matches" $ first16bitsMatch 1431495498 245556042 `shouldBe` True
    it "doesnt match" $ first16bitsMatch 1181022009 1233683848 `shouldBe` False

main :: IO ()
main = do
  hspec tests
  putStrLn $ "solution: " ++ show (solve input)
