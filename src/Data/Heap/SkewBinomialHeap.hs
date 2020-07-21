-- from "Purely Functional Data Structures" by Chris Okasaki

module Data.Heap.SkewBinomialHeap (SkewBinomialHeap) where

import Data.Heap.Heap

data Tree a = NODE Int a [a] [Tree a]

newtype SkewBinomialHeap a = SBH [Tree a]

rank :: Tree a -> Int
rank (NODE r _ _ _) = r

root :: Tree a -> a
root (NODE _ x _ _) = x

link :: Ord a => Tree a -> Tree a -> Tree a
link t1@(NODE r x1 xs1 c1) t2@(NODE _ x2 xs2 c2) = if x1 <= x2
  then NODE (r + 1) x1 xs1 (t2:c1)
  else NODE (r + 1) x2 xs2 (t1:c2)

skewLink :: Ord a => a -> Tree a -> Tree a -> Tree a
skewLink x t1 t2 = if x <= y
  then NODE r x (y:ys) c
  else NODE r y (x:ys) c
  where NODE r y ys c = link t1 t2

insTree :: Ord a => Tree a -> [Tree a] -> [Tree a]
insTree x []         = [x]
insTree x tts@(t:ts) = if rank x < rank t
  then x:tts
  else insTree (link x t) ts

mrg :: Ord a => [Tree a] -> [Tree a] -> [Tree a]
mrg ts1 [] = ts1
mrg [] ts2 = ts2
mrg tts1@(t1:ts1) tts2@(t2:ts2)
  | rank t1 < rank t2 = t1 : mrg ts1 tts2
  | rank t2 < rank t1 = t2 : mrg tts1 ts2
  | otherwise         = insTree (link t1 t2) (mrg ts1 ts2)

normalize :: Ord a => [Tree a] -> [Tree a]
normalize []     = []
normalize (t:ts) = insTree t ts

removeMinTree :: Ord a => [Tree a] -> (Tree a, [Tree a])
removeMinTree []     = error "Empty heap"
removeMinTree [t]    = (t, [])
removeMinTree (t:ts) = if root t < root t' then (t, ts) else (t', t:ts')
  where (t', ts') = removeMinTree ts

instance Heap SkewBinomialHeap where
  empty = SBH []

  isEmpty (SBH ts) = null ts

  insert x (SBH (t1:t2:ts))
    | rank t1 == rank t2 = SBH (skewLink x t1 t2 : ts)
  insert x (SBH ts)      = SBH (NODE 0   x [] [] : ts)

  merge (SBH ts1) (SBH ts2) = SBH (mrg (normalize ts1) (normalize ts2))

  findMin (SBH ts) = root t where (t, _) = removeMinTree ts

  deleteMin (SBH ts) = foldr insert (SBH ts') xs where
    (NODE _ _ xs ts1, ts2) = removeMinTree ts
    ts'                    = mrg (reverse ts1) (normalize ts2)
