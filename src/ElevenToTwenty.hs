module ElevenToTwenty where

import Util (problem11Const)
import Data.List (transpose)

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