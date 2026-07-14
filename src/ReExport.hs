module ReExport (allPuzzles) where

import OneToTen (oneToTen)
import Data.Map (union, Map)
import ElevenToTwenty (elevenToTwenty)

allPuzzles :: Map String Integer
allPuzzles = oneToTen `union` elevenToTwenty