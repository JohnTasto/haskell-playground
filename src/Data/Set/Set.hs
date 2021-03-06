-- from "Purely Functional Data Structures" by Chris Okasaki

{-# LANGUAGE MultiParamTypeClasses #-}

module Data.Set.Set (Set (..)) where

class Set s a where
  empty  :: s a
  insert :: a -> s a -> s a
  member :: a -> s a -> Bool
