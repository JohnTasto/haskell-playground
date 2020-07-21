-- from "Purely Functional Data Structures" by Chris Okasaki

{-# LANGUAGE MultiParamTypeClasses #-}

module Data.FiniteMap.FiniteMap (FiniteMap (..)) where

class FiniteMap m k where
  empty  :: m k a
  bind   :: k -> a -> m k a -> m k a
  lookup :: k -> m k a -> Maybe a
