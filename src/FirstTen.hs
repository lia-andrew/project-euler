module FirstTen (
  problem1,
  problem2,
  problem3,
  problem4,
  problem5,
  problem6) where

import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Bits ((.&.))
import Util

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

problem4 :: Integer
problem4 = maximum [z | x <- [100..999], y <- [100..999], let z = x * y, let a = show z, a == reverse a]

problem5 :: Integer
problem5 = product $ map (uncurry (^)) $ Map.toList $ foldr (\x y -> Map.unionWith max y $ toMap $ primeFactorize x 2) Map.empty [2..20]
    where
      toMap xs = foldr (\x y -> Map.insert x (length . filter (==x) $ xs) y) Map.empty $ Set.fromList xs

problem6 :: Integer
problem6 = sum [1..100] ^ 2 - sum (map (^2) [1..100])