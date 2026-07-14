module ElevenToTwenty (elevenToTwenty) where

import Util (problem11Const, primeFactorize)
import Data.List (transpose, subsequences, nub)
import qualified Data.Map as Map (fromList, Map)

elevenToTwenty :: Map.Map String Integer
elevenToTwenty = Map.fromList [("11", problem11), ("12", problem12)]

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