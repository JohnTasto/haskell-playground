-- from "Purely Functional Data Structures" by Chris Okasaki

module Data.Queue.BankersQueue (BankersQueue) where

import Prelude hiding (head, tail)

import Data.Queue.Queue

data BankersQueue a = BQ Int [a] Int [a]

check :: Int -> [a] -> Int -> [a] -> BankersQueue a
check lenf f lenr r = if lenf < lenr
  then BQ (lenf + lenr) (f ++ reverse r) 0 []
  else BQ lenf f lenr r

instance Queue BankersQueue where
  empty = BQ 0 [] 0 []

  isEmpty (BQ lenf _ _ _) = lenf == 0

  -- O(1) amortized persistent
  snoc (BQ lenf f lenr r) x = check lenf f (lenr + 1) (x:r)

  -- O(1)
  head (BQ _ []    _ _) = error "Empty queue"
  head (BQ _ (n:_) _ _) = n

  -- O(1) amortized persistent
  tail (BQ _    []    _    _) = error "Empty queue"
  tail (BQ lenf (_:f) lenr r) = check (lenf - 1) f lenr r


-- Invariant:  D(i) <= min(2i, |f|-|r|)

--                    . . .  . . .
--   i                0 1 2  5 4 3
-- d(i)               0 0 0  0 0 0
-- D(i)               0 0 0  0 0 0
-- min(2i, |f|-|r|)   0 0 0  0 0 0
--
--     snoc               . . . . . . .               . . . . . . .
--       i                0 1 2 3 4 5 6      ->       0 1 2 3 4 5 6
--     d(i)               1 1 1 4 0 0 0  discharge 1  0 1 1 4 0 0 0
--     D(i)               1 2 3 7 7 7 7      ->       0 1 2 6 6 6 6
--     min(2i, |f|-|r|)   0 2 4 6 7 7 7               0 2 4 6 7 7 7
--
--         snoc               . . . . . . .  .               . . . . . . .  .
--           i                0 1 2 3 4 5 6  7      ->       0 1 2 3 4 5 6  7
--         d(i)               0 1 1 4 0 0 0  0  discharge 1  0 0 1 4 0 0 0  0
--         D(i)               0 1 2 6 6 6 6  6      ->       0 0 1 5 5 5 5  5
--         min(2i, |f|-|r|)   0 2 4 6 6 6 6  6               0 2 4 6 6 6 6  6
--
--         tail               . . . . . .                   . . . . . .
--           i                0 1 2 3 4 5        ->         0 1 2 3 4 5
--         d(i)               1 1 4 0 0 0    discharge 2    0 0 4 0 0 0
--         D(i)               1 2 6 6 6 6        ->         0 0 4 4 4 4
--         min(2i, |f|-|r|)   0 2 4 6 6 6                   0 2 4 6 6 6
--
--     tail               . . . . .                   . . . . .
--       i                0 1 2 3 4        ->         0 1 2 3 4
--     d(i)               1 1 3 0 0    discharge 2    0 0 3 0 0
--     D(i)               1 2 5 5 5        ->         0 0 3 3 3
--     min(2i, |f|-|r|)   0 2 4 5 5                   0 2 4 5 5
