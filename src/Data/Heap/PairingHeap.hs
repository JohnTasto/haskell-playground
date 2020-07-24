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

  -- O(1), O(log n) amortized ephemeral (possibly still O(1))
  insert x = merge (T x [])

  -- O(1), O(log n) amortized ephemeral (possibly still O(1))
  merge h E = h
  merge E h = h
  merge h1@(T n1 cs1) h2@(T n2 cs2) = if n1 < n2
    then T n1 (h2:cs1)
    else T n2 (h1:cs2)

  -- O(1)
  findMin  E      = error "Empty heap"
  findMin (T n _) = n

  -- O(n), O(log n) amortized ephemeral
  deleteMin  E       = error "Empty heap"
  deleteMin (T _ cs) = mergePairs cs
