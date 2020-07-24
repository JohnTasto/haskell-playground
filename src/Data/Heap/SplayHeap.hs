-- from "Purely Functional Data Structures" by Chris Okasaki

module Data.Heap.SplayHeap (SplayHeap) where

import Data.Heap.Heap

data SplayHeap a = E | T (SplayHeap a) a (SplayHeap a)

partition :: Ord a => a -> SplayHeap a -> (SplayHeap a, SplayHeap a)
partition _    E        = (E, E)
partition x h@(T l n r) = if x >= n
  then case r of
    E          -> (h, E)
    T rl rn rr -> if x >= rn
      then let (rrl, rrr) = partition x rr in (T (T l n rl) rn rrl, rrr)
      else let (rll, rlr) = partition x rl in (T l n rll, T rlr rn rr)
  else case l of
    E          -> (E, h)
    T ll ln lr -> if x >= ln
      then let (lrl, lrr) = partition x lr in (T ll ln lrl, T lrr n r)
      else let (lll, llr) = partition x ll in (lll, T llr ln (T lr n r))

instance Heap SplayHeap where
  empty = E

  isEmpty E = True
  isEmpty _ = False

  -- O(log n) amortized ephemeral
  insert x h = T l x r
    where (l, r) = partition x h

  -- O(n)
  merge  E        h = h
  merge (T l n r) h = T (merge hl l) n (merge hr r)
    where (hl, hr) = partition n h

  -- O(n)
  findMin  E        = error "Empty heap"
  findMin (T E n _) = n
  findMin (T l _ _) = findMin l

  -- O(log n) amortized ephemeral
  deleteMin  E                   = error "Empty heap"
  deleteMin (T  E           _ r) = r
  deleteMin (T (T E  _  lr) n r) = T lr n r
  deleteMin (T (T ll ln lr) n r) = T (deleteMin ll) ln (T lr n r)
