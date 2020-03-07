{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.MultiIntSetSpec
  ( spec,
  )
where

-- , CoSerial(..)
-- , newtypeAlts

import Control.Arrow ((***))
import Data.IntMap as IntMap hiding (fromList, singleton, toList)
import Data.IntSet as IntSet (fromAscList)
-- import Control.Exception (assert)

-- , insert
-- , map

-- , toSetWithFilter

import Data.List (nub)
import Data.MultiIntSet as MIS
  ( IsList (..),
    MultiIntSet (..),
    bind,
    fromSet,
    singleton,
  )
import Test.Hspec
import Test.SmallCheck ((==>))
import Test.SmallCheck.Series
  ( NonNegative (..),
    Positive,
    Serial (..),
    (\/),
    cons2,
    getNonNegative,
    getPositive,
  )
import Test.Utils

-- instance
--   (Monad m, CoSerial m x)
--   => CoSerial m (IntMap x) where
--     coseries bSeries =
--       assert False
--       ((\ f -> f . IntMap.toList) <$> coseries bSeries)

-- make a Map to act as a function to test bind
instance
  (Monad m) =>
  Serial m (IntMap MultiIntSet)
  where
  series = pure IntMap.empty \/ cons2 mkForBind

mkForBind :: -- make a Map to act as a function to test bind
  (NonNegative Int, MultiIntSet) ->
  [(Positive Int, MultiIntSet)] ->
  IntMap MultiIntSet
mkForBind (i, x) =
  IntMap.fromAscList
    . makeMonotonic (i, x)
    . fmap (first getPositive)

mkPositiveIntMap ::
  (NonNegative Int, Positive x) ->
  [(Positive Int, Positive x)] ->
  IntMap x
mkPositiveIntMap (i, x) =
  IntMap.fromAscList
    . makeMonotonic (i, getPositive x)
    . stripPositiveWrapers

makeMonotonic ::
  (NonNegative Int, x) -> -- smallest key in the map
  [(Int, x)] -> -- step to the next key paired with the value
  [(Int, x)] -- an ascending list of key value pairs
makeMonotonic (i, x) =
  scanl
    (\(a, _) (c, d) -> (a + c, d))
    (i', x)
  where
    i' :: Int
    i' = getNonNegative i

stripPositiveWrapers ::
  [(Positive Int, Positive x)] ->
  [(Int, x)]
stripPositiveWrapers = fmap (getPositive *** getPositive)

mkNonNegativeIntSet ::
  NonNegative Int -> -- smallest value in the set
  [Positive Int] -> -- list of steps to the next value
  IntSet
mkNonNegativeIntSet (NonNegative start) =
  IntSet.fromAscList
    . scanl (+) start
    . fmap getPositive

instance (Monad m) => Serial m MultiIntSet where
  series =
    MultiIntSet
      <$> (pure IntMap.empty \/ cons2 mkPositiveIntMap)

instance (Monad m) => Serial m IntSet where
  series = pure mempty \/ cons2 mkNonNegativeIntSet

{- HLINT ignore spec "Monoid law, left identity" -}

spec :: Spec
spec =
  describe "MultiIntSet:" $ do
    describe "is a \"unsigned\" cancelative comunative Monoid" $ do
      prop "<> is associative" $
        \(a :: MultiIntSet, b, c) ->
          a <> (b <> c) `shouldBe` (a <> b) <> c
      prop "mempty is left idenity" $
        \(a :: MultiIntSet) ->
          mempty <> a `shouldBe` a
      prop "<> is communative" $
        \(a :: MultiIntSet) b ->
          a <> b `shouldBe` b <> a
      prop "is unsigned (has no inverses)" $
        \(a :: MultiIntSet) b ->
          a <> b == mempty ==> a `shouldBe` mempty
      prop "is cancelative" $
        \(a :: MultiIntSet, (b, c)) ->
          a <> b == a <> c ==> b `shouldBe` c
    -- Note - We use intmaps to properly limit the number of different functions. Otherwise just a depth of 3 tests millions of cases.
    describe "bind and singleton form a 'mono'Monad" $ do
      prop "left idenity" $
        \(NonNegative (n :: Int)) intmap ->
          do
            let f ix = findWithDefault mempty ix intmap
            (singleton n `bind` f) `shouldBe` f n
      prop "right idenity" $
        \(a :: MultiIntSet) ->
          (a `bind` singleton) `shouldBe` a
      prop "associativity" $
        \(a :: MultiIntSet, intmapF, intmapG) ->
          do
            let f ix = findWithDefault mempty ix intmapF
            let g ix = findWithDefault mempty ix intmapG
            ((a `bind` f) `bind` g)
              `shouldBe` (a `bind` (\x -> f x `bind` g))
    describe "MultiIntSet is a quotient of [Int]" $ do
      prop "fromList . toList is id" $
        \(a :: MultiIntSet) ->
          fromList (MIS.toList a) `shouldBe` a
      prop "toList . fromList is sort" $
        \(a :: [Int]) ->
          let m :: MultiIntSet = fromList a
           in MIS.toList m `shouldBe` sort a
    describe "IntSet is a quotient of MultiIntSet" $ do
      prop "fromList . toList . fromSet is id" $
        \(a :: IntSet) ->
          MIS.fromList (MIS.toList (MIS.fromSet a)) `shouldBe` a
      prop "toList . fromSet . fromList is nub . sort" $
        \(list :: [Int]) ->
          MIS.toList (MIS.fromSet (MIS.fromList list))
            `shouldBe` nub (sort list)
