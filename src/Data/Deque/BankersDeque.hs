-- from "Purely Functional Data Structures" by Chris Okasaki

module Data.Deque.BankersDeque (BankersDeque) where

import Prelude hiding (head,tail,last,init)

import Data.Deque.Deque

data BankersDeque a = BD Int [a] Int [a]

c :: Int
c = 3

check :: Int -> [a] -> Int -> [a] -> BankersDeque a
check lenf f lenr r
  | lenf > c*lenr + 1 = let i  = (lenf + lenr) `div` 2
                            j  = lenf + lenr - i
                            f' = take i f
                            r' = r ++ reverse (drop i f)
                        in  BD i f' j r'
  | lenr > c*lenf + 1 = let j  = (lenf + lenr) `div` 2
                            i  = lenf + lenr - j
                            r' = take j r
                            f' = f ++ reverse (drop j r)
                        in  BD i f' j r'
  | otherwise         = BD lenf f lenr r

instance Deque BankersDeque where
  empty = BD 0 [] 0 []

  isEmpty (BD lenf _ lenr _) = lenf + lenr == 0

  cons x (BD lenf f lenr r) = check (lenf + 1) (x:f) lenr r

  head (BD _ []    _ _) = error "Empty deque"
  head (BD _ (x:_) _ _) = x

  tail (BD _    []     _    _) = error "Empty deque"
  tail (BD lenf (_:f') lenr r) = check (lenf - 1) f' lenr r

  snoc (BD lenf f lenr r) x = check lenf f (lenr + 1) (x:r)

  last (BD _ _ _ []   ) = error "Empty deque"
  last (BD _ _ _ (x:_)) = x

  init (BD _    _ _    []    ) = error "Empty deque"
  init (BD lenf f lenr (_:r')) = check lenf f (lenr - 1) r'
