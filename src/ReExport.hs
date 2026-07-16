module ReExport (allPuzzles) where

import Data.Map (union, Map)
import From1To10 (from1To10)
import From11To20 (from11To20)

allPuzzles :: Map String Integer
allPuzzles = from1To10 `union` from11To20