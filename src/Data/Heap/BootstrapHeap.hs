-- from "Purely Functional Data Structures" by Chris Okasaki

module Data.Heap.BootstrapHeap (BootstrapHeap) where

import Data.Heap.Heap

data BootstrapHeap h a = E | H a (h (BootstrapHeap h a))

instance Eq a => Eq (BootstrapHeap h a) where
  (H x1 _) == (H x2 _) = x1 == x2
  _        == _        = error "Some error not in book"

instance Ord a => Ord (BootstrapHeap h a) where
  (H x1 _) <= (H x2 _) = x1 <= x2
  _        <= _        = error "Some error not in book"

instance Heap h => Heap (BootstrapHeap h) where
  empty = E

  isEmpty E = True
  isEmpty _ = False

  insert x = merge (H x empty)

  merge E h = h
  merge h E = h
  merge h1@(H x1 p1) h2@(H x2 p2) = if x1 <= x2
    then H x1 (insert h2 p1)
    else H x2 (insert h1 p2)

  findMin  E      = error "Empty heap"
  findMin (H x _) = x

  deleteMin E = error "Empty heap"
  deleteMin (H _ p) = if isEmpty p
    then E
    else case findMin p of
      H x p1 -> H x (merge p1 p2)
      E      -> error "Some error not in book"
      where p2 = deleteMin p
