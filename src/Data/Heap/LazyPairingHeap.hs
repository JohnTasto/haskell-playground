-- from "Purely Functional Data Structures" by Chris Okasaki

module Data.Heap.LazyPairingHeap (PairingHeap) where

import Data.Heap.Heap

data PairingHeap a = E | T a (PairingHeap a) (PairingHeap a)

link :: Ord a => PairingHeap a -> PairingHeap a -> PairingHeap a
link (T x E m) a = T x a m
link (T x b m) a = T x E (merge (merge a b) m)
link  E        _ = error "Some error not in book"

instance Heap PairingHeap where
  empty = E

  isEmpty E = True
  isEmpty _ = False

  insert x = merge (T x E E)

  merge a E = a
  merge E b = b
  merge a@(T x _ _) b@(T y _ _) = if x <= y then link a b else link b a

  findMin E = error "Empty heap"
  findMin (T x _ _) = x

  deleteMin E = error "Empty heap"
  deleteMin (T _ a m) = merge a m
