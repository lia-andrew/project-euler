module Util where

primeFactorize :: Integral t => t -> t -> [t]
primeFactorize x y
  | x == 1 = []
  | modulo == 0 = y : primeFactorize quotient 2
  | otherwise = primeFactorize x (y + 1)
  where
    (quotient, modulo) = divMod x y