-- From https://wiki.haskell.org/GHC/Type_families

module Main where

import Data.Maybe (fromMaybe)
import GMap

myGMap :: GMap (Int, Either Char ()) String
myGMap
  = insert (5, Left 'c') "(5, Left 'c')"
  $ insert (4, Right ()) "(4, Right ())"
  $ insert (5, Right ()) "This is the one!"
  $ insert (5, Right ()) "This is the two!"
  $ insert (6, Right ()) "(6, Right ())"
  $ insert (5, Left 'a') "(5, Left 'a')"
  empty

main :: IO ()
main = putStrLn $ fromMaybe "Couldn't find key!" $ GMap.lookup (5, Right ()) myGMap
