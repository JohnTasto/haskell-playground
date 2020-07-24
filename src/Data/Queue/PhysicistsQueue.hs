-- from "Purely Functional Data Structures" by Chris Okasaki

module Data.Queue.PhysicistsQueue (PhysicistsQueue) where

import Prelude hiding (head, tail)
import qualified Data.List (tail)
import Data.Queue.Queue

data PhysicistsQueue a = PQ [a] Int [a] Int [a]

check :: [a] -> Int -> [a] -> Int -> [a] -> PhysicistsQueue a
check w lenf f lenr r = if lenf < lenr
  then checkw f (lenf + lenr) (f ++ reverse r) 0 []
  else checkw w lenf f lenr r

checkw :: [a] -> Int -> [a] -> Int -> [a] -> PhysicistsQueue a
checkw [] lenf f lenr r = PQ f lenf f lenr r
checkw w  lenf f lenr r = PQ w lenf f lenr r

instance Queue PhysicistsQueue where
  empty = PQ [] 0 [] 0 []

  isEmpty (PQ _ lenf _ _ _) = lenf == 0

  -- O(1) amortized persistent
  snoc (PQ w lenf f lenr r) x = check w lenf f (lenr + 1) (x:r)

  -- O(1)
  head (PQ []    _ _ _ _) = error "Empty queue"
  head (PQ (n:_) _ _ _ _) = n

  -- O(1) amortized persistent
  tail (PQ []    _    _ _    _) = error "Empty queue"
  tail (PQ (_:w) lenf f lenr r) = check w (lenf - 1) (Data.List.tail f) lenr r
