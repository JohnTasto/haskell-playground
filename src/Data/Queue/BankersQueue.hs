-- from "Purely Functional Data Structures" by Chris Okasaki

module Data.Queue.BankersQueue (BankersQueue) where

import Prelude hiding (head, tail)

import Data.Queue.Queue

data BankersQueue a = BQ Int [a] Int [a]

check :: Int -> [a] -> Int -> [a] -> BankersQueue a
check lenf f lenr r = if lenr <= lenf
  then BQ lenf f lenr r
  else BQ (lenf + lenr) (f ++ reverse r) 0 []

instance Queue BankersQueue where
  empty = BQ 0 [] 0 []

  isEmpty (BQ lenf _ _ _) = lenf == 0

  snoc (BQ lenf f lenr r) x = check lenf f (lenr + 1) (x:r)

  head (BQ _ []    _ _) = error "Empty queue"
  head (BQ _ (x:_) _ _) = x

  tail (BQ _    []     _    _) = error "Empty queue"
  tail (BQ lenf (_:f') lenr r) = check (lenf - 1) f' lenr r
