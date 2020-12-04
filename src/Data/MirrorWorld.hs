{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}

module Data.MirrorWorld
  ( World (..),
    WorldSize (..),
    WorldState (..),
    dimension,
    neighborIndexes,
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

data OneWayDir = N | NE | SE | S | SW | NW
  deriving stock (Show, Read, Eq, Ord, Ix)

data TwoWayDir = NS | NESW | SENW
  deriving stock (Eq, Show, Read)

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
      { dotsB :: IntMap CellState
      }
  deriving stock (Show, Read, Eq)

newtype WorldSize = WS {unWS :: NonEmpty Int}
  deriving newtype (Show, Read, Eq)

dimension :: WorldSize -> Int
dimension (WS sizeList) = NonEmpty.length sizeList

numEntries :: WorldSize -> Int
numEntries (WS sizeList) = Prelude.product sizeList

moveDots :: WorldSize -> WorldState -> WorldState
moveDots size oldState =
  let oldDots = dots oldState
   in WorldState {dots = oldDots // [(dir, IntSet.map . moveDot dir $ oldDots ! dir) | dir <- range (bounds oldDots)]}

onDir NS = (N, S)
onDir NESW = (NE, SW)
onDir SENW = (SE, NW)

-- Remember the direction is the direction the particles are moving towards! (Not where they come from!)
mirrorDir NS = (SE, SW) :| [(NE, NW)]
mirrorDir NESW = (N, SE) :| [(S, NW)]
mirrorDir SENW = (N, SW) :| [(S, NE)]

interact :: World -> WorldState -> WorldState
interact world oldState =
  toWorldState $ interact' world (toWorldStateB oldState)

toWorldStateB :: WorldState -> WorldStateB
toWorldStateB = error "Stub"

toWorldState :: WorldStateB -> WorldState
toWorldState = error "Stub"

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
