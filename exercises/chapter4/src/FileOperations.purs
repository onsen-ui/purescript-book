module FileOperations where

import Prelude

import Control.MonadZero (guard)
import Data.Array (all, concatMap, filter, (:))
import Data.Path (Path, isDirectory, ls)

allFiles :: Path -> Array Path
allFiles root = root : concatMap allFiles (ls root)

allFiles' :: Path -> Array Path
allFiles' file = file : do
  child <- ls file
  allFiles' child

onlyFiles :: Path -> Array Path
onlyFiles = filter (not isDirectory) <<< allFiles
