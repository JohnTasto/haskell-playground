-- from "Purely Functional Data Structures" by Chris Okasaki

module Data.Heap.LeftistHeap (LeftistHeap) where

import Data.Heap.Heap

data LeftistHeap a = E | T Int a (LeftistHeap a) (LeftistHeap a)

rank :: LeftistHeap a -> Int
rank E = 0
rank (T r _ _ _) = r

makeT :: a -> LeftistHeap a -> LeftistHeap a -> LeftistHeap a
makeT x h1 h2 = if rh1 >= rh2
  then T (rh2 + 1) x h1 h2
  else T (rh1 + 1) x h2 h1
  where
    rh1 = rank h1
    rh2 = rank h2

instance Heap LeftistHeap where
  empty = E

  isEmpty E = True
  isEmpty _ = False

  -- O(log n)
  insert x = merge (T 1 x E E)

  -- O(log n)
  merge l E = l
  merge E r = r
  merge h1@(T _ x1 l1 r1) h2@(T _ x2 l2 r2) = if x1 <= x2
    then makeT x1 l1 (merge r1 h2)
    else makeT x2 l2 (merge h1 r2)

  -- O(1)
  findMin  E          = error "Empty heap"
  findMin (T _ x _ _) = x

  -- O(log n)
  deleteMin  E          = error "Empty heap"
  deleteMin (T _ _ l r) = merge l r
