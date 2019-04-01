module FileOperations where

import Prelude

import Control.MonadZero (guard)
import Data.Array (concatMap, foldl, (:))
import Data.Maybe (Maybe(..))
import Data.Path (Path, ls, isDirectory, size, root, fileName)

allFiles' :: Path -> Array Path
allFiles' file = file : concatMap allFiles' (ls file)

allFiles :: Path -> Array Path
allFiles file = file : do
  child <- ls file
  allFiles child

onlyFiles :: Path -> Array Path
onlyFiles path = do
  file <- allFiles path
  guard $ isDirectory >>> not $ file
  pure file

largestFile :: Path -> Int
largestFile = largestFile' <<< onlyFiles 
  where
    largestFile' :: Array Path -> Int
    largestFile' = foldl largerFile (bottom :: Int)

    largerFile :: Int -> Path -> Int
    largerFile s p = case size p of
      Nothing -> s
      Just s' -> max s s'

largestSize :: Int
largestSize = largestFile root

smallestFile :: Path -> Int
smallestFile = smallestFile' <<< onlyFiles 
  where
    smallestFile' :: Array Path -> Int
    smallestFile' = foldl smallerFile (top :: Int)

    smallerFile :: Int -> Path -> Int
    smallerFile s p = case size p of
      Nothing -> s
      Just s' -> min s s'

smallestSize :: Int
smallestSize = smallestFile root

-- TODO : this
-- whereIs :: String -> Maybe Path
-- whereIs filename = do
--   fs <- onlyFiles root
--   fnames <- map fileName fs


