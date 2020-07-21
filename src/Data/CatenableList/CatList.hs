-- from "Purely Functional Data Structures" by Chris Okasaki

module Data.CatenableList.CatList (CatList) where

import Prelude hiding (head, tail, (++))
import Data.CatenableList.CatenableList
import Data.Queue.Queue (Queue)
import qualified Data.Queue.Queue as Queue

data CatList q a = E | C a (q (CatList q a))

link :: Queue q => CatList q a -> CatList q a -> CatList q a
link (C x q) s = C x (Queue.snoc q s)
link  E      _ = error "Some error not in book"

instance Queue q => CatenableList (CatList q) where
  empty = E

  isEmpty E = True
  isEmpty _ = False

  xs ++ E  = xs
  E  ++ ys = ys
  xs ++ ys = link xs ys

  cons x xs = C x Queue.empty ++ xs

  snoc xs x = xs ++ C x Queue.empty

  head  E      = error "Empty list"
  head (C x _) = x

  tail  E      = error "Empty list"
  tail (C _ q) = if Queue.isEmpty q then E else linkAll q
    where
      linkAll q' = if Queue.isEmpty q'' then t else link t (linkAll q'')
        where
          t   = Queue.head q'
          q'' = Queue.tail q'
