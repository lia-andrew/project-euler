module Util where

import Data.Char (digitToInt)

primeFactorize :: Integral t => t -> t -> [t]
primeFactorize x y
  | x == 1 = []
  | remainder == 0 = y : primeFactorize quotient 2
  | otherwise = primeFactorize x (y + 1)
  where
    (quotient, remainder) = divMod x y

primes :: Integral t => [t]
primes = 2 : 3 : 5 : eratosSieve (candidates 1 $ cycle [6, 4, 2, 4, 2, 4, 6, 2])
  where
    candidates x (y:ys) = z : candidates z ys
      where
        z = x + y
    eratosSieve (x:xs) = x : eratosSieve (xs \\ (square : lazyConcatSort [square + 2 * x, square + 4 * x..] [square + 3 * x, square + 6 * x..] [square + 5 * x, square + 10 * x..]))
      where
        square = x * x
        lazyConcatSort a@(x:xs) b@(y:ys) c@(z:zs) = case compare x y of 
            LT -> case compare x z of
                LT -> x : lazyConcatSort xs b c -- x < y && x < z
                EQ -> x : lazyConcatSort xs b zs -- (x = z) < y
                GT -> z : lazyConcatSort a b zs -- z < x < y
            EQ -> case compare x z of
                LT -> x : lazyConcatSort xs ys c -- (x = y) < z
                EQ -> x : lazyConcatSort xs ys zs -- x = y = z
                GT -> z : lazyConcatSort a ys zs -- z < (x = y)
            GT -> case compare y z of
                LT -> y : lazyConcatSort a ys c -- y < x && y < z
                EQ -> y : lazyConcatSort a ys zs -- (y = z) < x
                GT -> z : lazyConcatSort a b zs -- z < y < x
        (\\) a@(x:xs) b@(y:ys) = case compare x y of
            LT -> x : (\\) xs b
            EQ -> (\\) xs ys
            GT -> (\\) a ys

problem8Const :: [Integer]
problem8Const = map (toInteger . digitToInt) $ show 7316717653133062491922511967442657474235534919493496983520312774506326239578318016984801869478851843858615607891129494954595017379583319528532088055111254069874715852386305071569329096329522744304355766896648950445244523161731856403098711121722383113622298934233803081353362766142828064444866452387493035890729629049156044077239071381051585930796086670172427121883998797908792274921901699720888093776657273330010533678812202354218097512545405947522435258490771167055601360483958644670632441572215539753697817977846174064955149290862569321978468622482839722413756570560574902614079729686524145351004748216637048440319989000889524345065854122758866688116427171479924442928230863465674813919123162824586178664583591245665294765456828489128831426076900422421902267105562632111110937054421750694165896040807198403850962455444362981230987879927244284909188845801561660979191338754992005240636899125607176060588611646710940507754100225698315520005593572972571636269561882670428252483600823257530420752963450