module ElevenToTwenty (elevenToTwenty) where

import Data.List (transpose, subsequences, nub, foldl')
import Util (problem11Const, primeFactorize, problem13Const)
import qualified Data.Map as Map (fromList, Map, lookup, insert, singleton, foldrWithKey', empty)

elevenToTwenty :: Map.Map String Integer
elevenToTwenty = Map.fromList [("11", problem11), ("12", problem12), ("13", problem13), ("14", problem14), ("15", problem15)]

problem11 :: Integer
problem11 = maximum [maxLine problem11Const, maxLine $ transpose problem11Const, maxDiag problem11Const, maxDiag $ reverse problem11Const]
  where
    maxLine = foldr (\line best -> max best $ processLine line) 0
    processLine xs
      | length xs < 4 = 0
      | otherwise = max (product $ take 4 xs) $ processLine $ tail xs
    maxDiag xs
      | length xs < 4 = 0
      | otherwise = max (processGroup $ take 4 xs) $ maxDiag $ tail xs
    processGroup [a, b, c, d] = maximum [(a !! i) * (b !! (i + 1)) * (c !! (i + 2)) * (d !! (i + 3)) | i <- [0..16]]

problem12 :: Integer
problem12 = head . filter (\x -> numFactors x > 500) . triangleNums 1 $ 2
  where
  triangleNums x y = x : triangleNums (x + y) (y + 1)
  numFactors = length . nub . map product . subsequences . primeFactorize 2

problem13 :: Integer
problem13 = total `div` (10 ^ (numDigits - 9))
  where
    total = sum problem13Const
    (numDigits, _) = properFraction $ logBase 10 $ fromIntegral total

problem14 :: Integer
problem14 = fst . Map.foldrWithKey' longestPath (0, 0) . foldl' (\cache x -> collatz x x 1 cache) (Map.singleton 1 4) $ [2..999999]
  where
    collatz x 1 z cache = Map.insert x z cache
    collatz x y z cache = case Map.lookup y cache of
      Just val -> Map.insert x (val + z - 1) cache
      Nothing
        | even y -> collatz x (y `div` 2) (z + 1) cache
        | otherwise -> collatz x (3 * y + 1) (z + 1) cache
    longestPath key val prev@(_, prevVal)
      | val > prevVal = (key, val)
      | otherwise = prev

problem15 :: Integer -- puzzle is symmetric, so you could compute only half then multiply by 2
problem15 = fst $ countPaths (0, 0) Map.empty
  where
    countPaths pos@(x, y) cache = case Map.lookup pos cache of
      Just cachedCount -> (cachedCount, cache)
      Nothing -> (newCount, newCache)
      where
        countChildPath childPos edgeCheck childCache
          | edgeCheck == 19 = (1, childCache)
          | otherwise = case Map.lookup childPos childCache of
            Just cachedCount -> (cachedCount, childCache)
            Nothing -> countPaths childPos childCache
        (rightCount, rightCache) = countChildPath (x + 1, y) x cache
        (downCount, downCache) = countChildPath (x, y + 1) y rightCache
        newCount = rightCount + downCount
        newCache = Map.insert pos newCount downCache