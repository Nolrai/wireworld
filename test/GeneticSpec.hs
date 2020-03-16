{-# LANGUAGE TemplateHaskellQuotes #-}

module GeneticSpec
  ( spec,
  )
where

import Genetic
  ( genetic,
    initialize,
    mkWorld,
    runWithTestInputs,
    scoreGenome,
  )
import Test.Hspec
import Test.Utils

spec :: Spec
spec =
  do
    describe (show 'genetic) $ do
      xprop "does things" stub
      xprop "does things" stub
    describe (show 'initialize) $ do
      xprop "does things" stub
      xprop "does things" stub
    describe (show 'mkWorld) $ do
      xprop "does things" stub
      xprop "does things" stub
    describe (show 'runWithTestInputs) $ do
      xprop "does things" stub
      xprop "does things" stub
    describe (show 'scoreGenome) $ do
      xprop "does things" stub
      xprop "does things" stub

stub :: Bool -> Bool
stub = fix id
