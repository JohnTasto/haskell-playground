-- from "Purely Functional Data Structures" by Chris Okasaki

module Data.Heap.PairingHeap (PairingHeap) where

import Data.Heap.Heap

data PairingHeap a = E | T a [PairingHeap a]

mergePairs :: Ord a => [PairingHeap a] -> PairingHeap a
mergePairs [] = E
mergePairs [h] = h
mergePairs (h1:h2:hs) = merge (merge h1 h2) (mergePairs hs)

instance Heap PairingHeap where
  empty = E

  isEmpty E = True
  isEmpty _ = False

  insert x = merge (T x [])

  merge h E = h
  merge E h = h
  merge h1@(T x hs1) h2@(T y hs2) = if x < y
    then T x (h2:hs1)
    else T y (h1:hs2)

  findMin  E      = error "Empty heap"
  findMin (T x _) = x

  deleteMin  E       = error "Empty heap"
  deleteMin (T _ hs) = mergePairs hs
