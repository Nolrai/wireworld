{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.MultiIntSet
  ( MultiIntSet (..),
    bind,
    fromSet,
    IsList (..),
    insert,
    map,
    singleton,
    toSetWithFilter,
  )
where

import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet
import Data.List (foldl')
import GHC.Exts
import Prelude hiding (empty, fromList, map)

newtype MultiIntSet = MultiIntSet {unMultiIntSet :: IntMap Int}
  deriving newtype (Eq, Ord, Show, Read)

instance Semigroup MultiIntSet where
  (<>) = add

instance Monoid MultiIntSet where
  mempty = MultiIntSet IntMap.empty

add :: MultiIntSet -> MultiIntSet -> MultiIntSet
add a b =
  MultiIntSet $
    IntMap.unionWith
      (+)
      (unMultiIntSet a)
      (unMultiIntSet b)

-- this is also the pure of the monotyped Monad.
singleton :: Int -> MultiIntSet
singleton key = MultiIntSet $ IntMap.singleton key 1

insert :: Int -> MultiIntSet -> MultiIntSet
insert x (MultiIntSet intmap) =
  MultiIntSet $ IntMap.insertWith (+) x 1 intmap

-- the bind for a monotyped Monad.
bind :: MultiIntSet -> (Int -> MultiIntSet) -> MultiIntSet
bind (MultiIntSet intmap) f =
  IntMap.foldMapWithKey
    (\key value -> liftMIS (IntMap.map (value *)) $ f key)
    intmap

liftMIS :: (IntMap Int -> IntMap Int) -> MultiIntSet -> MultiIntSet
liftMIS f = MultiIntSet . f . unMultiIntSet

fromSet :: IntSet -> MultiIntSet
fromSet = MultiIntSet . IntMap.fromSet (const 1)

instance IsList MultiIntSet where
  type Item MultiIntSet = Int
  fromList l = Data.List.foldl' add mempty $ fmap singleton l
  toList = IntMap.foldMapWithKey (flip replicate) . unMultiIntSet

-- monotyped fmap
map :: (Int -> Int) -> MultiIntSet -> MultiIntSet
map f m = bind m (singleton . f)

toSetWithFilter :: (Int -> Int -> Bool) -> MultiIntSet -> IntSet
toSetWithFilter f (MultiIntSet intmap) =
  IntMap.foldMapWithKey
    ( \key value ->
        if f key value
          then IntSet.singleton key
          else IntSet.empty
    )
    intmap
