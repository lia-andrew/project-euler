module FirstTen (
  problem1,
  problem2) where

import Data.Bits ((.&.))

problem1 :: Integer
problem1 = sum . filter isMultiple . takeWhile (< 1000) $ [1..]
  where
    isMultiple x = (x `mod` 3 == 0) || (x `mod` 5 == 0)

problem2 :: Integer
problem2 = sum $ takeWhile (<= 4000000) $ evenFibonacci 1 1
  where
    evenFibonacci x y
      | next .&. 1 == 0 = next : recur
      | otherwise = recur
      where
        next = x + y
        recur = evenFibonacci y next