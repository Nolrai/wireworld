{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}


module WireWorld
  ( World (..)
  , WorldSize (..)
  , dimension
  , neighborIndexes
  )
where
import Data.Vector.Unboxed as V


data World = World
  { heads :: IntSet
  , tails :: IntSet
  , metal :: Vector Bool
  }
  deriving stock (Show, Read, Eq)

newtype WorldSize = WS {unWS :: [Int]}
  deriving newtype (Show, Read, Eq)

dimension :: WorldSize -> Int
dimension (WS sizeList) = Prelude.length sizeList

-- This leads to a slightly wierd topology at the edges but I don't care
neighborIndexes :: WorldSize -> Int -> [Int]
neighborIndexes sizeList
  = \ ix -> ((`mod` size) . (ix+)) <$> Prelude.drop 1 neighbors
  where
  neighbors = neighbors' . unWS $ sizeList
  size = Prelude.product . unWS $ sizeList

neighbors' :: [Int] -> [Int]
neighbors' [] = pure 0
neighbors' (_:xs) =
    do
    oldIndex <- neighbors' xs
    offset <- [0,1,-1] --weird order is so we know the first item is 0
    pure $ oldIndex * Prelude.product xs + offset