module FileOperations where

import Prelude

import Data.Array (concatMap, filter, foldr, null, (:))
import Data.Array.Partial (head)
import Data.Maybe (Maybe(..))
import Data.Path (Path, filename, isDirectory, ls, size, root)
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

whereIs :: String -> Maybe Path
whereIs query = if null $ find root
  then Nothing
  else Just $ unsafePartial head $ find root
    where
      find :: Path -> Array Path
      find file = do
        child <- ls file
        if filename child == query then pure file else find child
