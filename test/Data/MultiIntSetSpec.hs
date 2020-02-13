{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.MultiIntSetSpec (spec) where

import Test.Hspec
import Test.Hspec.SmallCheck as HSC
import Test.SmallCheck.Series
  ( Serial(..)
  -- , CoSerial(..)
  -- , newtypeAlts
  , cons2
  , getNonNegative
  , NonNegative (..)
  , getPositive
  , Positive
  , (\/)
  , getDepth
  , list
  , Series
  )
import Test.SmallCheck
  ( (==>)
  , over
  , Property
  )
import Data.IntMap as IntMap hiding (singleton)
import Control.Arrow ((***))
-- import Control.Exception (assert)

import Data.MultiIntSet as MIS
  ( MultiIntSet(..)
  , bind
  -- , empty
  -- , fromList
  -- , fromSet
  -- , insert
  -- , map
  , singleton
  -- , toSetWithFilter
  )
import Test.Utils

-- instance
--   (Monad m, CoSerial m x)
--   => CoSerial m (IntMap x) where
--     coseries bSeries =
--       assert False
--       ((\ f -> f . IntMap.toList) <$> coseries bSeries)

instance
  (Monad m)
  => Serial m (IntMap MultiIntSet) where
  series = pure IntMap.empty \/ cons2 mkForBind

mkForBind
  :: (NonNegative Int, MultiIntSet)
  -> [(Positive Int, MultiIntSet)]
  -> IntMap MultiIntSet
mkForBind (i,x) =
  IntMap.fromAscList
  . makeMonotonic (i,x)
  . fmap (first getPositive)

mkPositiveIntMap
  :: (NonNegative Int, Positive x)
  -> [(Positive Int, Positive x)]
  -> IntMap x
mkPositiveIntMap (i,x) =
  IntMap.fromAscList
  . makeMonotonic (i, getPositive x)
  . stripPositiveWrapers

makeMonotonic
  :: (NonNegative Int, x)
  -> [(Int, x)] -> [(Int, x)]
makeMonotonic (i,x) =
    scanl
      (\ (a, _) (c, d) -> (a+c, d))
      (i', x)
  where
  i' :: Int
  i' = getNonNegative i

stripPositiveWrapers
  :: [(Positive Int, Positive x)]
  -> [(Int, x)]
stripPositiveWrapers = fmap (getPositive *** getPositive)

instance (Monad m) => Serial m MultiIntSet where
  series =
    MultiIntSet
      <$> (pure IntMap.empty \/ cons2 mkPositiveIntMap)

-- instance (Monad m) => CoSerial m MultiIntSet where
--   coseries b = assert False $ newtypeAlts b

-- xit :: String -> a -> SpecWith ()
-- xit str _ = it str pending

spec :: Spec
spec =
  do
  describe "MultiIntSet:" $ do
    describe "is a \"unsigned\" comunative Monoid" $ do
      prop "<> is associative" $
        \ (a :: MultiIntSet, b, c)
          -> a <> (b <> c) `shouldBe` (a <> b) <> c
      prop "mempty is left idenity" $
        \ (a :: MultiIntSet)
          -> mempty <> a `shouldBe` a
      prop "<> is communative" $
        \ (a :: MultiIntSet) b
          -> a <> b `shouldBe` b <> a
      prop "is unsigned (has no inverses)" $
        \ (a :: MultiIntSet) b
          -> a <> b == mempty ==> a `shouldBe` mempty

    -- Note - We use intmaps to properly limit the number of different functions. Otherwise just a depth of 3 tests millions of cases.
    describe "bind and singleton form a 'mono'Monad" $ do
      prop "left idenity" $
        \ (NonNegative (n :: Int) ) intmap
          ->
            do
            let f ix = findWithDefault mempty ix intmap
            (singleton n `bind` f) `shouldBe` f n
      prop "right idenity" $
        \ (a :: MultiIntSet)
          -> (a `bind` singleton) `shouldBe` a
      prop "associativity" . property $
        \ (a :: MultiIntSet, intmapF, intmapG)
          ->
            do
            let f ix = findWithDefault mempty ix intmapF
            let g ix = findWithDefault mempty ix intmapG
            ((a `bind` f) `bind` g)
              `shouldBe` (a `bind` (\x -> f x `bind` g))