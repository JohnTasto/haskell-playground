-- from "Purely Functional Data Structures" by Chris Okasaki

module Data.Sortable.BottomUpMergeSort (MergeSort) where

import Data.Sortable.Sortable

data MergeSort a = MS Int [[a]]

mrg :: Ord a => [a] -> [a] -> [a]
mrg [] ys = ys
mrg xs [] = xs
mrg xxs@(x:xs) yys@(y:ys) = if x <= y
  then x : mrg xs yys
  else y : mrg xxs ys

instance Sortable MergeSort where
  empty = MS 0 []

  add x (MS size segs) = MS (size + 1) (addSeg [x] segs size)
    where
      addSeg seg segs' size' = if size' `mod` 2 == 0
        then seg:segs'
        else addSeg (mrg seg (head segs')) (tail segs') (size' `div` 2)

  sort (MS _ segs) = foldl mrg [] segs
