-- from "Practical Haskell 2e" by Alejandro Serrano Mena

{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

module KMeans where

import Data.List
import qualified Data.Map as M

class Ord v => Vector v where
  distance :: v -> v -> Double
  centroid :: [v] -> v

instance Vector (Double, Double) where
--distance :: (Double, Double) -> (Double, Double) -> Double
  distance (a1, b1) (a2, b2) = sqrt $ a * b where
    a = a2 - a1
    b = b2 - b1

--centroid :: [(Double, Double)] -> (Double, Double)
  centroid lst = (a/n, b/n) where
    (a, b) = foldr (\(a1, b1) (a2, b2) -> (a1 + a2, b1 + b2)) (0, 0) lst
    n      = fromIntegral $ length lst

class Vector v => Vectorizable e v where
  toVector :: e -> v

instance Vectorizable (Double, Double) (Double, Double) where
--toVector :: (Double, Double) -> (Double, Double)
  toVector = id

clusterAssignmentPhase :: (Vectorizable e v) => [v] -> [e] -> M.Map v [e]
clusterAssignmentPhase centroids = foldr xxx initialMap where

  -- xxx :: e -> M.Map v [e] -> M.Map v [e]
  xxx p = M.adjust (p:) $ minimumBy (compareDistance p) centroids

  -- compareDistance :: (Vectorizable e v) => e -> v -> v -> Ordering
  compareDistance p x y = compare (distance x $ toVector p) (distance y $ toVector p)

  initialMap = M.fromList $ zip centroids (repeat [])
