-- from "Purely Functional Data Structures" by Chris Okasaki

module Data.RandomAccessList.AltBinaryRandomAccessList (BinaryList) where

import Prelude hiding (head, tail, lookup)
import Data.RandomAccessList.RandomAccessList

data BinaryList a = Nil | ZERO (BinaryList (a, a)) | ONE a (BinaryList (a, a))

uncons :: BinaryList a -> (a, BinaryList a)
uncons  Nil        = error "Empty list"
uncons (ONE x Nil) = (x, Nil)
uncons (ONE x ps)  = (x, ZERO ps)
uncons (ZERO  ps)  = let ((x,y), ps') = uncons ps in (x, ONE y ps')

fupdate :: (a -> a) -> Int -> BinaryList a -> BinaryList a
fupdate _ _  Nil       = error "Bad subscript"
fupdate f 0 (ONE x ps) = ONE (f x) ps
fupdate f i (ONE x ps) = cons x (fupdate f (i - 1) (ZERO ps))
fupdate f i (ZERO  ps) = ZERO (fupdate f' (i `div` 2) ps)
  where f' (x, y) = if i `mod` 2 == 0 then (f x, y) else (x, f y)

instance RandomAccessList BinaryList where
  empty = Nil

  isEmpty Nil = True
  isEmpty _   = False

  cons x  Nil       = ONE x Nil
  cons x (ZERO  ps) = ONE x ps
  cons x (ONE y ps) = ZERO (cons (x, y) ps)

  head xs = fst (uncons xs)

  tail xs = snd (uncons xs)

  lookup _  Nil       = error "Bad subscript"
  lookup 0 (ONE x _ ) = x
  lookup i (ONE _ ps) = lookup (i - 1) (ZERO ps)
  lookup i (ZERO  ps) = if i `mod` 2 == 0 then x else y
    where (x, y) = lookup (i `div` 2) ps

  update i y = fupdate (const y) i
