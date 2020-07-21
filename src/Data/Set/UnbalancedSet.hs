-- from "Purely Functional Data Structures" by Chris Okasaki

{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

module Data.Set.UnbalancedSet (UnbalancedSet) where

import Data.Set.Set

data UnbalancedSet a = E | T (UnbalancedSet a) a (UnbalancedSet a)

instance Ord a => Set UnbalancedSet a where
  empty = E

  member _ E    = False
  member x (T l n r)
    | x < n     = member x l
    | x > n     = member x r
    | otherwise = True

  insert x E    = T E x E
  insert x s@(T l n r)
    | x < n     = T (insert x l) n r
    | x > n     = T l n (insert x r)
    | otherwise = s
