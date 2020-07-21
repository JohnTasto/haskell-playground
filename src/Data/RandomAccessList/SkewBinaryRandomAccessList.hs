-- from "Purely Functional Data Structures" by Chris Okasaki

module Data.RandomAccessList.SkewBinaryRandomAccessList (SkewList) where

import Prelude hiding (head, tail, lookup)

import Data.RandomAccessList.RandomAccessList

data Tree a = LEAF a | NODE a (Tree a) (Tree a)

newtype SkewList a = SL [(Int, Tree a)]

instance RandomAccessList SkewList where
  empty = SL []

  isEmpty (SL ts) = null ts

  cons x (SL ((w1, t1) : (w2, t2) : ts))
    | w1 == w2   = SL ((1 + w1 + w2, NODE x t1 t2) : ts)
  cons x (SL ts) = SL ((1, LEAF x) : ts)

  head (SL [])                    = error "Empty list"
  head (SL ((1, LEAF x)     : _)) = x
  head (SL ((_, LEAF _)     : _)) = error "Some error not in book"
  head (SL ((_, NODE x _ _) : _)) = x

  tail (SL [])                       = error "Empty list"
  tail (SL ((1, LEAF _)       : ts)) = SL ts
  tail (SL ((_, LEAF _)       : _ )) = error "Some error not in book"
  tail (SL ((w, NODE _ t1 t2) : ts)) = SL ((w `div` 2, t1) : (w `div` 2, t2) : ts)

  lookup i (SL ts) = look i ts
    where
      look _  []           = error "Bad subscript"
      look i' ((w, t) : _) = if i' < w
        then lookTree w i' t
        else look (i' - w) ts
      lookTree 1 0  (LEAF x)       = x
      lookTree 1 _  (LEAF _)       = error "Bad subscript"
      lookTree _ _  (LEAF _)       = error "Some error not in book"
      lookTree _ 0  (NODE x _  _ ) = x
      lookTree w i' (NODE _ t1 t2) = if i' <= w'
        then lookTree w' (i' - 1)      t1
        else lookTree w' (i' - 1 - w') t2
        where w' = w `div` 2

  update i y (SL ts) = SL (upd i ts)
    where
      upd _ [] = error "Bad subscript"
      upd i' ((w, t) : _) = if i' < w
        then (w, updTree w i' t) : ts
        else (w, t) : upd (i' - w) ts
      updTree 1 0  (LEAF _)       = LEAF y
      updTree 1 _  (LEAF _)       = error "Bad subscript"
      updTree _ _  (LEAF _)       = error "Some error not in book"
      updTree _ 0  (NODE _ t1 t2) = NODE y t1 t2
      updTree w i' (NODE x t1 t2) = if i' < w'
        then NODE x (updTree w' (i' - 1) t1) t2
        else NODE x t1 (updTree w' (i' - 1 - w') t2)
        where w' = w `div` 2
