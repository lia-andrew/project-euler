module ReExport (allPuzzles) where

import OneToTen
import ElevenToTwenty
import Data.Map (union, Map)

allPuzzles :: Map String Integer
allPuzzles = oneToTen `union` elevenToTwenty