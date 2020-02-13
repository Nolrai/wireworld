{-# LANGUAGE FlexibleContexts #-}

module Test.Utils (prop) where

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
  , listM
  , Series
  )
import Test.SmallCheck
  ( (==>)
  , over
  , Property
  , Testable
  )
import Data.IntMap as IntMap hiding (singleton)
import Control.Arrow ((***))

testHowMany ::
  (Show a, Monad m) =>
  Series m a -> Property m
testHowMany someSeries =
  over getDepth $
    \ depth
      -> testHowMany' $ listM depth someSeries

testHowMany' :: Show a => [a] -> Either String String
testHowMany' values =
  if howMany > 10 ^ 6
    then Left $ errorMsg
    else Right $ show values ++ goodMsg
  where
  howMany :: Int
  howMany = length values
  errorMsg, goodMsg :: String
  errorMsg = "Too many: " ++ show howMany
  goodMsg = "Okay ammount " ++ show howMany

prop ::
  (Monad m, Show a, Serial IO a, Testable m b) =>
    String ->
    (a -> m b) ->
    Spec
prop name p =
  describe name $ do
    it "test how many?" . property $
      testHowMany (series :: Series m a)
    it "actual test" . property $ p

xprop ::
  (Monad m, Show a, Testable m b) =>
    String ->
    (a -> m b) ->
    Spec
xprop name p =
  it name pending