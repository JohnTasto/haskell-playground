-- from "Purely Functional Data Structures" by Chris Okasaki

module Data.CatenableDeque.CatenableDeque (CatenableDeque (..)) where
import Prelude hiding (head, tail, last, init, (++))
import Data.Deque.Deque

class Deque d => CatenableDeque d where
  (++) :: d a -> d a -> d a
