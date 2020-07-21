-- from "Purely Functional Data Structures" by Chris Okasaki

module Data.RandomAccessList.BinaryRandomAccessList (BinaryList) where

import Prelude hiding (head, tail, lookup)
import Data.RandomAccessList.RandomAccessList

data Tree a = LEAF a | NODE Int (Tree a) (Tree a)

data Digit a = ZERO | ONE (Tree a)

newtype BinaryList a = BL [Digit a]

size :: Tree a -> Int
size (LEAF _)     = 1
size (NODE w _ _) = w

link :: Tree a -> Tree a -> Tree a
link t1 t2 = NODE (size t1 + size t2) t1 t2

consTree :: Tree a -> [Digit a] -> [Digit a]
consTree t  []            = [ONE t]
consTree t  (ZERO   : ts) = ONE t : ts
consTree t1 (ONE t2 : ts) = ZERO  : consTree (link t1 t2) ts

unconsTree :: [Digit a] -> (Tree a, [Digit a])
unconsTree []           = error "Empty list"
unconsTree [ONE t]      = (t,  []          )
unconsTree (ONE t : ts) = (t,  ZERO   : ts )
unconsTree (ZERO  : ts) = case unconsTree ts of
  (NODE _ t1 t2, ts') -> (t1, ONE t2 : ts')
  (LEAF _,       _  ) -> error "Some error not in book"

instance RandomAccessList BinaryList where
  empty = BL []

  isEmpty (BL ts) = null ts

  cons x (BL ts) = BL (consTree (LEAF x) ts)

  head (BL ts) = case unconsTree ts of
    (LEAF x,     _) -> x
    (NODE _ _ _, _) -> error "Some error not in book"

  tail (BL ts) = BL ts' where (_, ts') = unconsTree ts

  lookup i (BL ts) = look i ts
    where
      look _  []           = error "Bad subscript"
      look i' (ZERO  : ts') = look i' ts'
      look i' (ONE t : ts') = if i' < size t
        then lookTree i' t
        else look (i' - size t) ts'
      lookTree 0  (LEAF x)        = x
      lookTree _  (LEAF _)        = error "Bad subscript"
      lookTree i' (NODE w t1 t2 ) = if i' < w `div` 2
        then lookTree i'               t1
        else lookTree (i' - w `div` 2) t2

  update i y (BL ts) = BL (upd i ts)
    where
      upd _  []           = error "Bad subscript"
      upd i' (ZERO  : ts') = ZERO : upd i' ts'
      upd i' (ONE t : ts') = if i' < size t
        then ONE (updTree i' t) : ts'
        else ONE t              : upd (i' - size t) ts'
      updTree 0  (LEAF _)        = LEAF y
      updTree _  (LEAF _)        = error "Bad subscript"
      updTree i' (NODE w t1 t2 ) = if i' < w `div` 2
        then NODE w (updTree i' t1) t2
        else NODE w t1              (updTree (i' - w `div` 2) t2)
