module Util where

primeFactorize :: Integral t => t -> t -> [t]
primeFactorize x y
  | x == 1 = []
  | modulo == 0 = y : primeFactorize quotient 2
  | otherwise = primeFactorize x (y + 1)
  where
    (quotient, modulo) = divMod x y

primes :: [Integer]
primes = 2 : eratos [3, 5..]
  where
    eratos (x:xs) = x : eratos (xs `lazyDifference` [x * x, x * x + 2 * x..])
    lazyDifference a@(x:xs) b@(y:ys) = case compare x y of
        LT -> x : lazyDifference xs b
        EQ -> lazyDifference xs ys
        GT -> lazyDifference a ys