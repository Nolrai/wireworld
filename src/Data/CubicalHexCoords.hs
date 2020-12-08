module Data.CubicalHexCoords where

import Linear.V3

newtype Cubical = Cubical (V3 Int)

data OneWayDir = N | NE | SE | S | SW | NW
  deriving stock (Show, Read, Eq, Ord, Ix)

data TwoWayDir = NS | NESW | SENW
  deriving stock (Eq, Show, Read)

toShift NW = Cubical (V3 0 (-1) 1)
toShift N = Cubical (V3 (-1) 0 1)
toShift NE = Cubical (V3 (-1) 1 0)
toShift SE = Cubical (V3 0 1 (-1))
toShift S = Cubical (V3 1 0 (-1))
toShift SW = Cubical (V3 1 (-1) 0)

onDir NS = (N, S)
onDir NESW = (NE, SW)
onDir SENW = (SE, NW)

moveCubical dir p = toShift dir .+ p

toIx (WS (rowSize, colSize)) (Cubical (V3 x y z)) = (col `mod` rowSize, (row `mod` colSize) * rowSize)
  where
    col = x
    row = z + (x `div` 2)

toCubical (WS (rowSize, colSize)) (col, row) = Cubical (V3 x y z)
  where
    x = col
    z = row - (col `div` 2)
    y = - (x + z)
