{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.MultiIntSet
  (MultiIntSet(..)
  , bind
  , empty
  , fromList
  , fromSet
  , insert
  , map
  , singleton
  , toSetWithFilter) where

import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet
import GHC.Exts
import Prelude hiding (empty, map, fromList)

newtype MultiIntSet = MultiIntSet {unMultiIntSet :: IntMap Int}
  deriving newtype (Eq, Ord, Show, Read)

instance Semigroup MultiIntSet where
  (<>) = add

instance Monoid MultiIntSet where
  mempty = empty

add :: MultiIntSet -> MultiIntSet -> MultiIntSet
add a b = MultiIntSet $ IntMap.unionWith (+) (unMultiIntSet a) (unMultiIntSet b)

empty :: MultiIntSet
empty = MultiIntSet IntMap.empty

singleton :: Int -> MultiIntSet
singleton key = MultiIntSet $ IntMap.singleton key 1

insert :: Int -> MultiIntSet -> MultiIntSet
insert x (MultiIntSet intmap) =
  MultiIntSet $ IntMap.insertWith (+) x 1 intmap

bind :: MultiIntSet -> (Int -> MultiIntSet) -> MultiIntSet
bind (MultiIntSet intmap) f =
  IntMap.foldMapWithKey
    (\key value -> liftMIS (IntMap.map (value*)) $ f key)
    intmap

liftMIS :: (IntMap Int -> IntMap Int) -> MultiIntSet -> MultiIntSet
liftMIS f = MultiIntSet . f . unMultiIntSet

fromSet :: IntSet -> MultiIntSet
fromSet = MultiIntSet . IntMap.fromSet (\_ -> 1)

instance IsList MultiIntSet where
  type (Item MultiIntSet) = Int
  fromList = fromSet . IntSet.fromList
  toList = IntMap.foldMapWithKey (flip replicate) . unMultiIntSet

map :: (Int -> Int) -> MultiIntSet -> MultiIntSet
map f m = bind m (singleton . f)

toSetWithFilter :: (Int -> Int -> Bool) -> MultiIntSet -> IntSet
toSetWithFilter f (MultiIntSet intmap) =
  IntMap.foldMapWithKey
    (\key value ->
      if f key value
        then IntSet.singleton key
        else IntSet.empty
    )
    intmap