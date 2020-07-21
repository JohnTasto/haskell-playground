-- from "Purely Functional Data Structures" by Chris Okasaki

{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

module Data.FiniteMap.Trie (Trie) where

import Prelude hiding (lookup)
import Data.Maybe (fromMaybe)
import Data.FiniteMap.FiniteMap

data Trie mk ks a = TRIE (Maybe a) (mk (Trie mk ks a))

instance FiniteMap m k => FiniteMap (Trie (m k)) [k] where
  empty = TRIE Nothing empty

  lookup []     (TRIE b _) = b
  lookup (k:ks) (TRIE _ m) = lookup k m >>= lookup ks

  bind []     x (TRIE _ m) = TRIE (Just x) m
  bind (k:ks) x (TRIE b m) = TRIE b $ bind k (bind ks x $ fromMaybe empty $ lookup k m) m
