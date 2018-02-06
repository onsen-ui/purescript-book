module FileOperations where

import Prelude

import Data.Array (concatMap, filter, foldr, (:))
import Data.Array.Partial (head)
import Data.Path (Path, filename, isDirectory, ls, size)
import Partial.Unsafe (unsafePartial)

allFiles :: Path -> Array Path
allFiles root = root : concatMap allFiles (ls root)

allFiles' :: Path -> Array Path
allFiles' file = file : do
  child <- ls file
  allFiles' child

onlyFiles :: Path -> Array Path
onlyFiles = filter (not isDirectory) <<< allFiles

smallestFile :: Path -> Path
smallestFile file = foldr compare (unsafePartial head files) files
  where
    files = onlyFiles file
    compare f min = if size f > size min then min else f

largestFile :: Path -> Path
largestFile file = foldr compare (unsafePartial head files) files
  where
    files = onlyFiles file
    compare f min = if size f < size min then min else f
