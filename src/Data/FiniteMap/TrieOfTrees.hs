-- from "Purely Functional Data Structures" by Chris Okasaki

{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

module Data.FiniteMap.TrieOfTrees (Tree (..), Trie) where

import Prelude hiding (lookup)
import Data.Maybe (fromMaybe)
import Data.FiniteMap.FiniteMap

data Tree a = E | T a (Tree a) (Tree a)

data Trie mk ks a = TRIE (Maybe a) (mk (Trie mk ks (Trie mk ks a)))

instance FiniteMap m k => FiniteMap (Trie (m k)) (Tree k) where
  empty = TRIE Nothing empty

  lookup  E        (TRIE v _) = v
  lookup (T k a b) (TRIE _ m) = lookup k m >>= lookup a >>= lookup b

  bind  E        x (TRIE _ m) = TRIE (Just x) m
  bind (T k a b) x (TRIE v m) = TRIE v $ bind k (bind a (bind b x $ fromMaybe empty $ lookup a t) t) m
    where t = fromMaybe empty (lookup k m)
