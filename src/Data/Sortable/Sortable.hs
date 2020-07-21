-- from "Purely Functional Data Structures" by Chris Okasaki

module Data.Sortable.Sortable (Sortable (..)) where

class Sortable s where
  empty :: Ord a => s a
  add   :: Ord a => a -> s a -> s a
  sort  :: Ord a => s a -> [a]
