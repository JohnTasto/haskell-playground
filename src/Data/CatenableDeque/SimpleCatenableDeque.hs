-- from "Purely Functional Data Structures" by Chris Okasaki

module Data.CatenableDeque.SimpleCatenableDeque (SimpleCatDeque) where

import Prelude hiding (head, tail, last, init, (++))
import Data.CatenableDeque.CatenableDeque
import Data.Deque.Deque

data SimpleCatDeque d a
  = SHALLOW (d a)
  | DEEP    (d a) (SimpleCatDeque d (d a)) (d a)

tooSmall :: Deque q => q a -> Bool
tooSmall d = isEmpty d || isEmpty (tail d)

dappendL :: (Deque q1, Deque q2) => q1 a -> q2 a -> q2 a
dappendL d1 d2 = if isEmpty d1 then d2 else cons (head d1) d2

dappendR :: (Deque q1, Deque q2) => q2 a -> q1 a -> q2 a
dappendR d1 d2 = if isEmpty d2 then d1 else snoc d1 (head d2)

instance Deque d => Deque (SimpleCatDeque d) where
  empty = SHALLOW empty

  isEmpty (SHALLOW d) = isEmpty d
  isEmpty _           = False

  cons x (SHALLOW d)  = SHALLOW (cons x d)
  cons x (DEEP f m r) = DEEP (cons x f) m r

  head (SHALLOW d)  = head d
  head (DEEP f _ _) = head f

  tail (SHALLOW d) = SHALLOW (tail d)
  tail (DEEP f m r)
    | not (tooSmall f') = DEEP f'                     m        r
    | isEmpty m         = SHALLOW (dappendL f' r)
    | otherwise         = DEEP (dappendL f' (head m)) (tail m) r
    where f' = tail f

  -- snoc, last, and init defined symmetrically...
  -- not sure it is correct

  snoc (SHALLOW d)  x = SHALLOW (snoc d x)
  snoc (DEEP f m r) x = DEEP (snoc f x) m r

  last (SHALLOW d)  = last d
  last (DEEP f _ _) = last f

  init (SHALLOW d) = SHALLOW (init d)
  init (DEEP f m r)
    | not (tooSmall f') = DEEP f'                     m        r
    | isEmpty m         = SHALLOW (dappendR f' r)
    | otherwise         = DEEP (dappendR f' (last m)) (init m) r
    where f' = init f

instance Deque d => CatenableDeque (SimpleCatDeque d) where
  (SHALLOW d1)    ++ (SHALLOW d2)
    | tooSmall d1                    = SHALLOW (dappendL d1 d2)
    | tooSmall d2                    = SHALLOW (dappendR d1 d2)
    | otherwise                      = DEEP d1             empty                      d2
  (SHALLOW d)     ++ (DEEP f m r)
    | tooSmall d                     = DEEP (dappendL d f) m                          r
    | otherwise                      = DEEP d              (cons f m)                 r
  (DEEP f m r)    ++ (SHALLOW d)
    | tooSmall d                     = DEEP f              m                          (dappendR r d)
    | otherwise                      = DEEP f              (snoc m r)                 d
  (DEEP f1 m1 r1) ++ (DEEP f2 m2 r2) = DEEP f1             (snoc m1 r1 ++ cons f2 m2) r2
