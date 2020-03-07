{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}

module Test.Utils
  ( prop,
    xprop,
    propWithLimit,
    xpropWithLimit,
  )
where

import Test.Hspec
import Test.Hspec.SmallCheck as HSC
-- , CoSerial(..)
-- , newtypeAlts

import Test.SmallCheck
  ( Testable,
    over,
  )
import Test.SmallCheck.Series
  ( Serial (..),
    limit,
  )

prop ::
  forall a b.
  (Show a, Serial IO a, Testable IO b) =>
  String ->
  (a -> b) ->
  Spec
prop name = propWithLimit name ((10 :: Int) ^ (6 :: Int))

propWithLimit ::
  forall a b.
  (Show a, Serial IO a, Testable IO b) =>
  String ->
  Int ->
  (a -> b) ->
  Spec
propWithLimit name nLimit p =
  describe name
    $ it "actual test" . property
    $ over (limit nLimit series) p

xprop ::
  forall a b.
  (Show a, Serial IO a, Testable IO b) =>
  String ->
  (a -> b) ->
  Spec
xprop name _ =
  it name pending

xpropWithLimit ::
  forall a b.
  (Show a, Serial IO a, Testable IO b) =>
  String ->
  Int ->
  (a -> b) ->
  Spec
xpropWithLimit name _ = xprop name
