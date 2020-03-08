{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}

module Data.WireWorld
  ( World (..),
    WorldSize (..),
    WorldState (..),
    dimension,
    neighborIndexes,
    step,
  )
where

import Control.Exception (assert)
import qualified Data.IntSet as IntSet
import Data.MultiIntSet as MIS
import Data.Vector.Unboxed

data World
  = World
      { metal :: Vector Bool,
        size :: WorldSize
      }
  deriving stock (Show, Read, Eq)

data WorldState
  = WorldState
      { headCells :: IntSet,
        tailCells :: IntSet
      }
  deriving stock (Show, Read, Eq)

instance Semigroup WorldState where
  (WorldState headCellsA tailCellsA) <> (WorldState headCellsB tailCellsB) =
    WorldState (headCellsA <> headCellsB) (tailCellsA <> tailCellsB)

instance Monoid WorldState where
  mempty = WorldState mempty mempty

newtype WorldSize = WS {unWS :: [Int]}
  deriving newtype (Show, Read, Eq)

dimension :: WorldSize -> Int
dimension (WS sizeList) = Prelude.length sizeList

-- This leads to a slightly wierd topology at the edges but I don't care
neighborIndexes :: WorldSize -> Int -> [Int]
neighborIndexes sizeList =
  \ix -> (`mod` size) . (ix +) <$> Prelude.drop 1 neighbors
  where
    neighbors = neighbors' . unWS $ sizeList
    size = Prelude.product . unWS $ sizeList

neighbors' :: [Int] -> [Int]
neighbors' [] = pure 0
neighbors' (_ : xs) =
  do
    oldIndex <- neighbors' xs
    offset <- [0, 1, -1] --weird order is so we know the first item is 0
    pure $ oldIndex * Prelude.product xs + offset

step :: World -> WorldState -> WorldState
step World {..} old =
  WorldState
    { tailCells = headCells old,
      headCells =
        toSetWithFilter
          (\value multiplicity -> (metal ! value) && valid multiplicity)
          (bind (MIS.fromSet $ headCells old) $ Prelude.fromList . neighborIndexes size)
          `IntSet.difference` (headCells old `IntSet.union` tailCells old)
    }
  where
    valid x = x == 1 || x == 2
