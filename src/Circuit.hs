-- Based on https://en.wikibooks.org/wiki/Haskell/Arrow_tutorial

{-# LANGUAGE Arrows, LambdaCase #-}
{- HLINT ignore "Avoid lambda" -}

module Circuit where

import Prelude hiding (id, (.))

import Control.Arrow
import Control.Monad
import Control.Category
import Data.List
import Data.Maybe
import System.Random


-- newtype Circuit a b = Circuit { unCircuit :: a -> (Circuit a b, b) }

newtype Circuit a b = Circuit (a -> (Circuit a b, b))

unCircuit :: Circuit a b -> a -> (Circuit a b, b)
unCircuit (Circuit cir) = cir


instance Category Circuit where
--id :: Circuit a a
  id = Circuit $ \ a -> (id, a)

--(.) :: Circuit b c -> Circuit a b -> Circuit a c
  Circuit f . Circuit g = Circuit $ \ a ->
    let (cirG', b) = g a
        (cirF', c) = f b
    in  (cirF' . cirG', c)


instance Arrow Circuit where
--arr :: (b -> c) -> Circuit b c
  arr f = Circuit (\ b -> (arr f, f b))

--first :: Circuit b c -> Circuit (b, d) (c, d)
  first (Circuit f) = Circuit $ \ (b, d) ->
    let (cir', c) = f b
    in  (first cir', (c, d))


instance ArrowLoop Circuit where
--loop :: Circuit (b, d) (c, d) -> Circuit b c
  loop (Circuit f) = Circuit $ \ b ->
    -- how can 'd' depend on itself here???
    let (cir', (c, d)) = f (b, d)
    in  (loop cir', c)


instance ArrowChoice Circuit where
--left :: Circuit b c -> Circuit (Either b d) (Either c d)
  left cir@(Circuit f) = Circuit $ \case
    Left b  -> (left cir', Left c) where (cir', c) = f b
    Right d -> (left cir, Right d)


runCircuit :: Circuit a b -> [a] -> [b]
-- 1 runCircuit _   []     = []
-- 1 runCircuit cir (x:xs) = x' : runCircuit cir' xs where (cir', x') = unCircuit cir x
-- 2 runCircuit cir xs = snd $ mapAccumL (\ cir x -> unCircuit cir x) cir xs
-- 3 runCircuit cir xs = snd $ mapAccumL unCircuit cir xs
runCircuit cir = snd . mapAccumL unCircuit cir


-- | Accumulator that outputs a value determined by the supplied function.
accum :: acc -> (a -> acc -> (b, acc)) -> Circuit a b
accum acc f = Circuit $ \ a -> let (b, acc') = f a acc in (accum acc' f, b)

-- | Accumulator that outputs the accumulator value.
accumV :: acc -> (a -> acc -> acc) -> Circuit a acc
accumV acc f = accum acc $ \ a acc' -> let acc'' = f a acc' in (acc'', acc'')


total :: Num a => Circuit a a
total = accumV 0 (+)

-- Main> runCircuit total [1,0,1,0,0,2]
-- [1,1,2,2,2,4]

mean1 :: Fractional a => Circuit a a
mean1 = (total &&& (const 1 ^>> total)) >>> arr (uncurry (/))

-- Main> runCircuit mean1 [0,10,7,8]
-- [0.0,5.0,5.666666666666667,6.25]

mean2 :: Fractional a => Circuit a a
mean2 = proc value -> do
  t <- total -< value
  n <- total -< 1
  returnA -< t / n

-- Main> runCircuit mean2 [0,10,7,8]
-- [0.0,5.0,5.666666666666667,6.25]

-- -- Boxed sums are not implemented
-- mean3 :: Fractional a => Circuit a a
-- mean3 = proc value -> do
--     (t, n) <- (| (&&&) (total -< value) (total -< 1) |)
--     returnA -< t / n

mean4 :: Fractional a => Circuit a a
mean4 = proc value -> do
  (t, n) <- (total -< value) &&& (total -< 1)
  returnA -< t / n

delay :: a -> Circuit a a
delay a = Circuit $ \ a' -> (delay a', a)

-- Main> runCircuit (delay 0) [5,6,7]
-- [0,5,6]

mean5 :: Fractional a => Circuit a a
mean5 = proc value -> do
  rec
    -- only order of arrow statements matter, and since
    -- there is only one, these lines can be in any order:
    (lastTot, lastN) <- delay (0, 0) -< (tot, n)
    let (tot, n) = (lastTot + value, lastN + 1)
    let mean = tot / n
  returnA -< mean


generator :: Random a => (a, a) -> StdGen -> Circuit () a
generator range rng = accum rng $ \ () rng' -> randomR range rng'

dictionary :: [[Char]]
dictionary = ["dog", "cat", "bird"]

pickWord :: StdGen -> Circuit () String
pickWord rng = proc () -> do
  idx <- generator (0, length dictionary - 1) rng -< ()
  returnA -< dictionary !! idx

-- Main> rng <- getStdGen
-- Main> runCircuit (pickWord rng) [(), (), ()]
-- ["dog","bird","dog"]

oneShot :: Circuit () Bool
oneShot = accum True $ \ _ acc -> (acc, False)

-- Main> runCircuit oneShot [(), (), (), (), ()]
-- [True,False,False,False,False]

delayedEcho :: a -> Circuit a a
-- delayedEcho acc = accum acc (\ a b -> (b,a))
delayedEcho acc = accum acc $ flip (,)

-- Main> runCircuit (delayedEcho False) [True, False, False, False, True]
-- [False,True,False,False,False]


getWord :: StdGen -> Circuit () String
getWord rng = proc () -> do
  -- If this is the first game loop, run pickWord. mPicked becomes Just <word>.
  -- On subsequent loops, mPicked is Nothing.
  firstTime <- oneShot -< ()
  mPicked <- if firstTime
    then do
      picked <- pickWord rng -< ()
      returnA -< Just picked
    else returnA -< Nothing
  -- An accumulator that retains the last 'Just' value.
  mWord <- accumV Nothing mplus -< mPicked
  returnA -< fromJust mWord

-- -- Illegal:
-- proc rng -> do
--   idx <- generator (0, length dictionary-1) rng -< ()  -- ILLEGAL
--   returnA -< dictionary !! idx

-- Main> rng <- getStdGen
-- Main> runCircuit (getWord rng) [(), (), (), (), (), ()]
-- ["dog","dog","dog","dog","dog","dog"]
