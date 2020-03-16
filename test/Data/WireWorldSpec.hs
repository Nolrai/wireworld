{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.WireWorldSpec
  ( spec,
  )
where

import qualified Data.IntSet as IntSet
import qualified Data.Vector.Unboxed as UV
import Test.Hspec as H
import Test.Hspec.SmallCheck as HSC
import Test.SmallCheck
import Test.SmallCheck.Series
import Test.Utils as Utils
import WireWorld
  ( World (..),
    WorldSize (..),
    WorldState (..),
    dimension,
    neighborIndexes,
    step,
  )

instance (Monad m) => Serial m WorldSize where
  series =
    do
      (NonNegative rank) <- series
      l <- replicateM (1 + rank) $
        do
          (NonNegative x) <- series
          pure (3 + x)
      (depth :: Int) <- getDepth
      guard (product l <= depth ^ (2 :: Int))
      pure (WS l)

instance (Monad m) => Serial m World where
  series = series >>- (\size -> UV.replicateM (product . unWS $ size) series >>- \metal -> pure World {..})

instance (Monad m) => Serial m (World, WorldState) where
  series =
    do
      world@World {..} <- series
      lists <- [0 .. UV.length metal - 1]
        `forM` \ix -> generate $
          \n ->
            case metal UV.!? ix of
              Just True -> rotate n [(Nothing, Nothing), (Just ix, Nothing), (Nothing, Just ix)]
              Just False -> [(Nothing, Nothing)]
              Nothing -> error $ " = " <> show ix <> " metal = " <> show metal
      let (headList, tailList) = (mapMaybe fst lists, mapMaybe snd lists)
      let worldState =
            WorldState
              (IntSet.fromAscList headList)
              (IntSet.fromAscList tailList)
      pure (world, worldState)
    where
      rotate n theList = go (n `mod` length theList) theList []
        where
          go 0 forward backward = forward ++ reverse backward
          go k (x : forward) backward = go (k - 1) forward (x : backward)
          go k [] backward = go k (reverse backward) [] --this shouldn't happen but..

validState :: World -> WorldState -> Bool
validState World {..} WorldState {..} =
  allInSet headCells isValidIx && allInSet tailCells isValidIx && IntSet.null (IntSet.intersection headCells tailCells)
  where
    allInSet set p = IntSet.foldr (\ix b -> p ix && b) True set
    isValidIx ix = all ($ix) [(>= 0), (< UV.length metal), (== Just True) . (metal UV.!?)]

spec :: Spec
spec =
  do
    describe "neighborIndexes" $ do
      it "produces valid indexes" $ HSC.property $
        \sizeList x ->
          all (\i -> i >= 0 && i < product (unWS sizeList)) $
            neighborIndexes sizeList x
      it "produces 3^dim - 1 values" $ HSC.property $
        \sizeList x ->
          (all (>= 3) . unWS) sizeList ==> length (neighborIndexes sizeList x) `shouldBe` (3 ^ dimension sizeList) - 1
    describe "series for (World, WorldState)" $
      Utils.propWithLimit
        "Produces valid states"
        (10 ^ (4 :: Int))
        (`shouldSatisfy` uncurry validState)
    describe "step" $ do
      Utils.propWithLimit "takes empty to empty" (10 ^ (4 :: Int)) $
        \world -> step world mempty `shouldBe` mempty
      Utils.propWithLimit "takes valid to valid" (10 ^ (4 :: Int)) $
        \(world, worldState) -> step world worldState `shouldSatisfy` validState world
