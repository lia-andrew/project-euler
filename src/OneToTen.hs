module OneToTen where

import Data.Bits ((.&.))
import qualified Data.Set as Set (fromList)
import Util (primeFactorize, primes, problem8Const)
import qualified Data.Map as Map (empty, insert, toList, unionWith)

problem1 :: Integer
problem1 = sum . filter (\x -> x `mod` 3 == 0 || x `mod` 5 == 0) . takeWhile (< 1000) $ [1..]

problem2 :: Integer
problem2 = sum . takeWhile (<= 4000000) . evenFibonacci 1 $ 1
  where
    evenFibonacci x y
      | next .&. 1 == 0 = next : recur
      | otherwise = recur
      where
        next = x + y
        recur = evenFibonacci y next

problem3 :: Integer
problem3 = maximum . primeFactorize 2 $ 600851475143

problem4 :: Integer
problem4 = maximum [z | x <- [100..999], y <- [100..999], let z = x * y, let a = show z, a == reverse a]

problem5 :: Integer
problem5 = product . map (uncurry (^)) . Map.toList . foldr (\x y -> Map.unionWith max y . toMap . primeFactorize 2 $ x) Map.empty $ [2..20]
    where
      toMap xs = foldr (\x y -> Map.insert x (length . filter (==x) $ xs) y) Map.empty $ Set.fromList xs

problem6 :: Integer
problem6 = sum [1..100] ^ 2 - (sum . map (^2) $ [1..100])

problem7 :: Integer
problem7 = primes !! 10000

problem8 :: Integer
problem8 = findLargest problem8Const 0
  where
    findLargest xs best
      | null xs = best
      | length xs < 13 = best
      | otherwise = findLargest (tail xs) $ max best $ product . take 13 $ xs

problem9 :: Integer
problem9 = head [x * y * z | x <- [1..1000], y <- [1..1000], x < y, let (integral, fraction) = properFraction . sqrt . fromInteger $ (x^2) + y^2, fraction == 0, let z = toInteger integral, x + y + z == 1000]

problem10 :: Integer
problem10 = sum . takeWhile (< 2000000) $ primes