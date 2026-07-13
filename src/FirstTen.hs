module FirstTen (problem1) where

problem1 :: Integer
problem1 = sum . filter isMultiple . takeWhile (< 1000) $ [1..]
  where
    isMultiple x = (x `mod` 3 == 0) || (x `mod` 5 == 0)