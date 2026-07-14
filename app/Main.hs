module Main where

import ReExport
import Data.Map (lookup)
import Prelude hiding (lookup)
import System.IO (stdout, hFlush)

main :: IO ()
main = do
    putStr "Enter puzzle number: "
    hFlush stdout
    input <- getLine
    putStrLn . extractVal . lookup input $ allPuzzles
  where
    extractVal (Just x) = show x
    extractVal Nothing = "That puzzle has not been implemented"