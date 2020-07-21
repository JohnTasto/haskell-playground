-- from "Purely Functional Data Structures" by Chris Okasaki

module Data.Queue.HoodMelvilleQueue (HoodMelvilleQueue) where

import Prelude hiding (head,tail)

import Data.Queue.Queue

data RotationState a
  = Idle
  | Reversing Int [a] [a] [a] [a]
  | Appending Int [a] [a]
  | Done [a]

data HoodMelvilleQueue a = HM Int [a] (RotationState a) Int [a]

exec :: RotationState a -> RotationState a
exec (Reversing ok (x:f) f' (y:r) r') = Reversing (ok + 1) f  (x:f') r (y:r')
exec (Reversing ok []    f' [y]   r') = Appending  ok      f' (y:r')
exec (Appending 0  _      r')         = Done r'
exec (Appending ok (x:f') r')         = Appending (ok - 1) f' (x:r')
exec state = state

invalidate :: RotationState a -> RotationState a
invalidate (Reversing ok f f' r r') = Reversing (ok - 1) f f' r r'
invalidate (Appending 0  _  (_:r')) = Done r'
invalidate (Appending ok f' r'    ) = Appending (ok - 1) f' r'
invalidate state = state

exec2 :: Int -> [a] -> RotationState a -> Int -> [a] -> HoodMelvilleQueue a
exec2 lenf f state lenr r = case exec (exec state) of
  Done newf -> HM lenf newf Idle     lenr r
  newstate  -> HM lenf f    newstate lenr r

check :: Int -> [a] -> RotationState a -> Int -> [a] -> HoodMelvilleQueue a
check lenf f state lenr r = if lenr <= lenf
  then exec2 lenf f state lenr r
  else let newstate = Reversing 0 f [] r []
       in  exec2 (lenf + lenr) f newstate 0 []

instance Queue HoodMelvilleQueue where
  empty = HM 0 [] Idle 0 []

  isEmpty (HM lenf _ _ _ _) = lenf == 0

  snoc (HM lenf f state lenr r) x = check lenf f state (lenr + 1) (x:r)

  head (HM _ []    _ _ _) = error "Empty queue"
  head (HM _ (x:_) _ _ _) = x

  tail (HM _    []     _     _    _) = error "Empty queue"
  tail (HM lenf (_:f') state lenr r) = check (lenf - 1) f' (invalidate state) lenr r
