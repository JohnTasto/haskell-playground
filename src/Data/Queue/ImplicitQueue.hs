-- from "Purely Functional Data Structures" by Chris Okasaki

module Data.Queue.ImplicitQueue (ImplicitQueue) where

import Prelude hiding (head, tail)

import Data.Queue.Queue

data Digit a = ZERO | ONE a | TWO a a

data ImplicitQueue a
  = SHALLOW (Digit a)
  | DEEP    (Digit a) (ImplicitQueue (a, a)) (Digit a)

instance Queue ImplicitQueue where
  empty = SHALLOW ZERO

  isEmpty (SHALLOW ZERO) = True
  isEmpty _              = False

  snoc (SHALLOW ZERO     )  y = SHALLOW (ONE y)
  snoc (SHALLOW (ONE x)  )  y = DEEP (TWO x y) empty ZERO
  snoc (SHALLOW (TWO _ _))  _ = error "Some error not in book"
  snoc (DEEP f m ZERO     ) y = DEEP f m (ONE y)
  snoc (DEEP f m (ONE x)  ) y = DEEP f (snoc m (x, y)) ZERO
  snoc (DEEP _ _ (TWO _ _)) _ = error "Some error not in book"

  head (SHALLOW ZERO     )  = error "Empty queue"
  head (SHALLOW (ONE x)  )  = x
  head (SHALLOW (TWO _ _))  = error "Some error not in book"
  head (DEEP ZERO      _ _) = error "Some error not in book"
  head (DEEP (ONE x)   _ _) = x
  head (DEEP (TWO x _) _ _) = x

  tail (SHALLOW ZERO     )  = error "Empty queue"
  tail (SHALLOW (ONE _)  )  = empty
  tail (SHALLOW (TWO _ _))  = error "Some error not in book"
  tail (DEEP ZERO      _ _) = error "Some error not in book"
  tail (DEEP (TWO _ y) m r) = DEEP (ONE y) m r
  tail (DEEP (ONE _)   m r) = if isEmpty m
    then SHALLOW r
    else DEEP (TWO y z) (tail m) r
      where (y, z) = head m
