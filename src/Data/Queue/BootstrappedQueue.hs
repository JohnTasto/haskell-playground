-- from "Purely Functional Data Structures" by Chris Okasaki

module Data.Queue.BootstrappedQueue (BootstrappedQueue) where

import Prelude hiding (head, tail)

import Data.Queue.Queue

data BootstrappedQueue a = E | Q Int [a] (BootstrappedQueue [a]) Int [a]

checkQ :: Int -> [a] -> BootstrappedQueue [a] -> Int -> [a] -> BootstrappedQueue a
checkQ lenfm f m lenr r = if lenr <= lenfm
  then checkF lenfm f m lenr r
  else checkF (lenfm + lenr) f (snoc m (reverse r)) 0 []

checkF :: Int -> [a] -> BootstrappedQueue [a] -> Int -> [a] -> BootstrappedQueue a
checkF _     [] E _    _ = E
checkF lenfm [] m lenr r = Q lenfm (head m) (tail m) lenr r
checkF lenfm f  m lenr r = Q lenfm f        m        lenr r

instance Queue BootstrappedQueue where
  empty = Q 0 [] E 0 []

  isEmpty E = True
  isEmpty _ = False

  snoc  E                   x = Q 1 [x] E 0 []
  snoc (Q lenfm f m lenr r) x = checkQ lenfm f m (lenr + 1) (x:r)

  head  E                = error "Empty queue"
  head (Q _ (x:_) _ _ _) = x
  head (Q _ []    _ _ _) = error "Some error not in book"

  tail  E                        = error "Empty queue"
  tail (Q lenfm (_:f') m lenr r) = checkQ (lenfm - 1) f' m lenr r
  tail (Q _     []     _ _    _) = error "Some error not in book"
