{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}

module Test.Utils
  ( prop,
    xprop,
  )
where

import Test.Hspec
import Test.Hspec.SmallCheck as HSC
-- , CoSerial(..)
-- , newtypeAlts

import Test.SmallCheck
  ( Property,
    Testable,
    monadic,
    over,
  )
import Test.SmallCheck.Series
  ( Serial (..),
    Series,
    getDepth,
    listM,
  )

testHowMany ::
  (Show a) =>
  Series IO a ->
  Property IO
testHowMany someSeries =
  over getDepth $
    \depth ->
      monadic $ testHowMany' <$> listM depth someSeries

testHowMany' ::
  Show a => [a] -> Either String String
testHowMany' values =
  if howMany > ((10 :: Int) ^ (6 :: Int))
    then Left errorMsg
    else Right $ show values ++ goodMsg
  where
    howMany :: Int
    howMany = length values
    errorMsg, goodMsg :: String
    errorMsg = "Too many: " ++ show howMany
    goodMsg = "Okay ammount " ++ show howMany

prop ::
  forall a b.
  (Show a, Serial IO a, Testable IO b) =>
  String ->
  (a -> b) ->
  Spec
prop name p =
  describe name $ do
    it "test how many?" $
      testHowMany (series :: Series IO a)
    it "actual test" . property $ p

xprop ::
  forall a b.
  (Show a, Serial IO a, Testable IO b) =>
  String ->
  (a -> b) ->
  Spec
xprop name _ =
  it name pending
