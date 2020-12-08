{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.MirrorWorld
  ( World (..),
    WorldSize (..),
    WorldState (..),
    dimension,
    step,
    numEntries,
  )
where

-- import Control.Exception (assert)

import Data.Array as Array
import Data.Bifunctor (second)
import qualified Data.IntMap as IntMap
import qualified Data.IntSet as IntSet
import Data.List.NonEmpty as NonEmpty
import HexCoords

type CellState = Array OneWayDir Bool

data World
  = World
      { mirrors :: IntMap TwoWayDir,
        size :: WorldSize
      }
  deriving stock (Show, Read, Eq)

newtype WorldState
  = WorldState
      { dots :: Array OneWayDir IntSet
      }
  deriving stock (Show, Read, Eq)

newtype WorldStateB
  = WorldStateB
      { dotsB :: Array (Int, Int) CellState
      }
  deriving stock (Show, Read, Eq)

data WorldSize = WS {rowSize :: Int, colSize :: Int}
  deriving stock (Show, Read, Eq, Ord, Ix)

moveDots :: WorldSize -> WorldState -> WorldState
moveDots size oldState =
  let oldDots = dots oldState
   in WorldState {dots = oldDots // [(dir, moveDot size dir `IntSet.map` oldDots ! dir) | dir <- range (bounds oldDots)]}

moveDot :: WorldSize -> OneWayDir -> IntSet.Key -> IntSet.Key
moveDot size dir = toIx size . moveCubical dir . toCubical size

-- Remember the direction is the direction the particles are moving towards! (Not where they come from!)
mirrorDir NS = (SE, SW) :| [(NE, NW)]
mirrorDir NESW = (N, SE) :| [(S, NW)]
mirrorDir SENW = (N, SW) :| [(S, NE)]

interact :: World -> WorldState -> WorldState
interact world oldState =
  toWorldState $ interact' world (toWorldStateB oldState)

toWorldStateB :: WorldState -> WorldStateB
toWorldStateB WorldState {..} = WorldStateB $
  array
    [
      (ix, array [(dir, ix `mem` (dots ! dir)) | dir <- range [minBound, maxBound])
      | ix <- bounds dots
    ]

toWorldState :: WorldStateB -> WorldState
toWorldState WorldStateB {..} = WorldState $
  array
    [
      (dir,

interact' :: World -> WorldStateB -> WorldStateB
interact' world =
  IntMap.update (swapArround (mirrors world))

swapArround :: IntMap TwoWayDir -> (Int, CellState) -> Maybe CellState
swapArround m (index, cellState) = pure $ swapArround' (IntMap.lookup m index) cellState

swapArround' :: Maybe TwoWayDir -> CellState -> CellState
swapArround' Nothing cellState = cellState
swapArround' (Just m) cellState =
  if uncurry ((==) `on` (cellState !)) (onDir m)
    then cellState // NonEmpty.toList (second (cellState !) <$> swaps)
    else cellState -- exactly one dot is flowing along the mirror
  where
    swaps :: NonEmpty (OneWayDir, OneWayDir)
    swaps = mirrorDir m <> (swap <$> mirrorDir m)
    swap (x, y) = (y, x)
