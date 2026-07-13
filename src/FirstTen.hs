module FirstTen (
  problem1,
  problem2,
  problem3,
  problem4) where

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

problem3 :: Integer
problem3 = maximum $ primeFactorize 600851475143 2
  where
    primeFactorize x y
      | x == 1 = []
      | modulo == 0 = y : primeFactorize quotient 2
      | otherwise = primeFactorize x (y + 1)
      where
        (quotient, modulo) = divMod x y

problem4 :: Integer
problem4 = maximum [z | x <- [100..999], y <- [100..999], let z = x * y, let a = show z, a == reverse a]