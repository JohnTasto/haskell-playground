-- from "Purely Functional Data Structures" by Chris Okasaki

module Data.Heap.BinomialHeap (BinomialHeap) where

import Data.Heap.Heap

data Tree a = T Int a [Tree a]        -- stored in decreasing rank order

newtype BinomialHeap a = BH [Tree a]  -- stored in increasing rank order

rank :: Tree a -> Int
rank (T r _ _) = r

root :: Tree a -> a
root (T _ x _) = x

-- ranks should be the same, but there is no check
link :: Ord a => Tree a -> Tree a -> Tree a
link t1@(T r x1 cs1) t2@(T _ x2 cs2) = if x1 <= x2
  then T (r + 1) x1 (t2:cs1)
  else T (r + 1) x2 (t1:cs2)

ins :: Ord a => Tree a -> [Tree a] -> [Tree a]
ins x []         = [x]
ins x tts@(t:ts) = if rank x < rank t
  then x:tts
  else ins (link x t) ts

mrg :: Ord a => [Tree a] -> [Tree a] -> [Tree a]
mrg ts1 [] = ts1
mrg [] ts2 = ts2
mrg tts1@(t1:ts1) tts2@(t2:ts2)
  | rank t1 < rank t2 = t1 : mrg ts1 tts2
  | rank t2 < rank t1 = t2 : mrg tts1 ts2
  | otherwise         = ins (link t1 t2) (mrg ts1 ts2)

removeMinTree :: Ord a => [Tree a] -> (Tree a, [Tree a])
removeMinTree []     = error "Empty heap"
removeMinTree [t]    = (t, [])
removeMinTree (t:ts) = if root t < root t'
  then (t, ts)
  else (t', t:ts')
  where (t', ts') = removeMinTree ts

instance Heap BinomialHeap where
  empty = BH []

  -- O(log n) since lazy? seems unlikely in Haskell with WHNF
  isEmpty (BH ts) = null ts

  -- O(log n), O(1) amortized persistent
  insert x (BH ts) = BH (ins (T 0 x []) ts)

  -- O(log n), O(log n) amortized persistent
  merge (BH ts1) (BH ts2) = BH (mrg ts1 ts2)

  -- O(log n), O(log n) amortized persistent
  findMin (BH ts) = root t
    where (t, _) = removeMinTree ts

  -- O(log n), O(log n) amortized persistent
  deleteMin (BH ts) = BH (mrg (reverse ts1) ts2)
    where (T _ _ ts1, ts2) = removeMinTree ts
