-- from "Purely Functional Data Structures" by Chris Okasaki

{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

module Data.Set.RedBlackSet (RedBlackSet) where

import Data.Set.Set

data Color = R | B

data RedBlackSet a = E | T Color (RedBlackSet a) a (RedBlackSet a)

balance :: Color -> RedBlackSet a -> a -> RedBlackSet a -> RedBlackSet a
balance B (T R (T R ll lx lr) cx rl) rx rr = T R (T B ll lx lr) cx (T B rl rx rr)
balance B (T R ll lx (T R lr cx rl)) rx rr = T R (T B ll lx lr) cx (T B rl rx rr)
balance B ll lx (T R (T R lr cx rl) rx rr) = T R (T B ll lx lr) cx (T B rl rx rr)
balance B ll lx (T R lr cx (T R rl rx rr)) = T R (T B ll lx lr) cx (T B rl rx rr)
balance color l x r = T color l x r

instance Ord a => Set RedBlackSet a where
  empty = E

  member _ E    = False
  member x (T _ l n r)
    | x < n     = member x l
    | x > n     = member x r
    | otherwise = True

  insert x s = case ins s of
    T _ l n r -> T B l n r
    E         -> error "Guaranteed to be non-empty"
    where
      ins E = T R E x E
      ins s'@(T color l' n' r')
        | x < n'    = balance color (ins l') n' r'
        | x > n'    = balance color l' n' (ins r')
        | otherwise = s'
