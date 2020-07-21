-- from "Purely Functional Data Structures" by Chris Okasaki

module Data.CatenableDeque.ImplicitCatenableDeque (Sized (..), ImplicitCatDeque) where

import Prelude hiding (head, tail, last, init, (++))
import Data.CatenableDeque.CatenableDeque
import Data.Deque.Deque

class Sized d where
  size :: d a -> Int

data ImplicitCatDeque d a
  = SHALLOW (d a)
  | DEEP    (d a) (ImplicitCatDeque d (CmpdElem d a)) (d a)
                  (ImplicitCatDeque d (CmpdElem d a)) (d a)

data CmpdElem d a
  = SIMPLE (d a)
  | CMPD   (d a) (ImplicitCatDeque d (CmpdElem d a)) (d a)

share :: (Deque q3, Deque q1, Deque q2) =>
           q1 a -> q2 a -> (q1 a, q3 a, q2 a)
share f r = (init f, m, tail r)
  where m = cons (last f) (cons (head r) empty)

dappendL :: (Deque q1, Deque q2) => q1 a -> q2 a -> q2 a
dappendL d1 d2 = if isEmpty d1
  then d2
  else dappendL (init d1) (cons (last d1) d2)

dappendR :: (Deque q1, Deque q2) => q2 a -> q1 a -> q2 a
dappendR d1 d2 = if isEmpty d2
  then d1
  else dappendR (snoc d1 (head d2)) (tail d2)

replaceHead :: Deque d =>
                 a -> ImplicitCatDeque d a -> ImplicitCatDeque d a
replaceHead x (SHALLOW d)      = SHALLOW (cons x (tail d))
replaceHead x (DEEP f a m b r) = DEEP (cons x (tail f)) a m b r

instance (Deque d, Sized d) => Deque (ImplicitCatDeque d) where
  empty = SHALLOW empty

  isEmpty (SHALLOW d) = isEmpty d
  isEmpty _           = False

  cons x (SHALLOW d)      = SHALLOW (cons x d)
  cons x (DEEP f a m b r) = DEEP (cons x f) a m b r

  head (SHALLOW d)      = head d
  head (DEEP f _ _ _ _) = head f

  tail (SHALLOW d)    = SHALLOW (tail d)
  tail (DEEP f a m b r)
    | size f > 3      = DEEP (tail f) a m b r
    | not (isEmpty a) = case head a of
        SIMPLE d      -> DEEP f' (tail a) m b r
          where f'  = dappendL (tail f) d
        CMPD f' c' r' -> DEEP f'' a'' m b r
          where f'' = dappendL (tail f) f'
                a'' = c' ++ replaceHead (SIMPLE r') a
    | not (isEmpty b) = case head b of
        SIMPLE d      -> DEEP f' empty d (tail b) r
          where f'  = dappendL (tail f) m
        CMPD f' c' r' -> DEEP f'' a'' r' (tail b) r
          where f'' = dappendL (tail f) m
                a'' = cons (SIMPLE f') c'
    | otherwise       = SHALLOW (dappendL (tail f) m) ++ SHALLOW r

  snoc (SHALLOW d)      x = SHALLOW (snoc d x)
  snoc (DEEP f a m b r) x = DEEP (snoc f x) a m b r

  last (SHALLOW d)      = last d
  last (DEEP f _ _ _ _) = last f

  init (SHALLOW d)    = SHALLOW (init d)
  init (DEEP f a m b r)
    | size f > 3      = DEEP (init f) a m b r
    | not (isEmpty a) = case last a of
        SIMPLE d      -> DEEP f' (init a) m b r
          where f' = dappendR (init f) d
        CMPD f' c' r' -> DEEP f'' a'' m b r
          where f'' = dappendR (init f) f'
                a'' = c' ++ replaceHead (SIMPLE r') a
    | not (isEmpty b) = case last b of
        SIMPLE d      -> DEEP f' empty d (init b) r
          where f' = dappendR (init f) m
        CMPD f' c' r' -> DEEP f'' a'' r' (init b) r
          where f'' = dappendR (init f) m
                a'' = snoc c' (SIMPLE f')
    | otherwise       = SHALLOW (dappendR (init f) m) ++ SHALLOW r

instance (Deque d, Sized d) => CatenableDeque (ImplicitCatDeque d) where
  (SHALLOW d1)          ++ (SHALLOW d2)
    | size d1 < 4                                = SHALLOW (dappendL d1 d2)
    | size d2 < 4                                = SHALLOW (dappendR d1 d2)
    | otherwise                                  = let (f, m, r) = share d1 d2
                                                   in  DEEP f empty m empty r
  (SHALLOW d)           ++ (DEEP f a m b r)
    | size d < 4                                 = DEEP (dappendL d f) a m b r
    | otherwise                                  = DEEP d (cons (SIMPLE f) a) m b r
  (DEEP f a m b r)      ++ (SHALLOW d)
    | size d < 4                                 = DEEP f a m b (dappendR r d)
    | otherwise                                  = DEEP f a m (snoc b (SIMPLE r)) d
  (DEEP f1 a1 m1 b1 r1) ++ (DEEP f2 a2 m2 b2 r2) = DEEP f1 a1' m b2' r2
    where
      (r1', m, f2') = share r1 f2
      a1'           = snoc a1 (CMPD m1 b1 r1')
      b2'           = cons (CMPD f2' a2 m2) b2
