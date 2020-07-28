-- From https://wiki.haskell.org/GHC/Type_families

{-# LANGUAGE TypeFamilies #-}

module GMap where

import Data.Char (ord)
import Data.Kind (Type)
import qualified Data.IntMap as IM

class GMapKey k where
  data GMap k :: Type -> Type
  empty       :: GMap k v
  lookup      :: k -> GMap k v -> Maybe v
  insert      :: k -> v -> GMap k v -> GMap k v

instance GMapKey Int where
  data GMap Int v        = GMapInt (IM.IntMap v)
  empty                  = GMapInt IM.empty
  lookup k   (GMapInt m) = IM.lookup k m
  insert k v (GMapInt m) = GMapInt (IM.insert k v m)

instance GMapKey Char where
  data GMap Char v        = GMapChar (GMap Int v)
  empty                   = GMapChar empty
  lookup k   (GMapChar m) = GMap.lookup (ord k) m
  insert k v (GMapChar m) = GMapChar (GMap.insert (ord k) v m)

instance GMapKey () where
  data GMap () v           = GMapUnit (Maybe v)
  empty                    = GMapUnit Nothing
  lookup ()   (GMapUnit v) = v
  insert () v (GMapUnit _) = GMapUnit $ Just v

instance (GMapKey a, GMapKey b) => GMapKey (a, b) where
  data GMap (a, b) v            = GMapPair (GMap a (GMap b v))
  empty                         = GMapPair empty
  lookup (a, b)   (GMapPair gm) = GMap.lookup a gm >>= GMap.lookup b
  insert (a, b) v (GMapPair gm) = GMapPair $ case GMap.lookup a gm of
    Nothing  -> insert a (insert b v empty) gm
    Just gm2 -> insert a (insert b v gm2  ) gm

instance (GMapKey a, GMapKey b) => GMapKey (Either a b) where
  data GMap (Either a b) v                = GMapEither (GMap a v) (GMap b v)
  empty                                   = GMapEither empty empty
  lookup (Left  a)   (GMapEither gm1 _  ) = GMap.lookup a gm1
  lookup (Right b)   (GMapEither _   gm2) = GMap.lookup b gm2
  insert (Left  a) v (GMapEither gm1 gm2) = GMapEither (insert a v gm1) gm2
  insert (Right b) v (GMapEither gm1 gm2) = GMapEither gm1 (insert b v gm2)
