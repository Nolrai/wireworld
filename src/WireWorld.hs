{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module WireWorld
  ( module Data.WireWorld,
    module TextDisplay,
    runSteps,
    runStepsWith,
  )
where

import Control.Monad.Writer
import Data.WireWorld
import TextDisplay

runSteps ::
  forall m t.
  (Monad m) =>
  (Text -> m ()) ->
  Maybe (m t) ->
  FromCell Text ->
  World ->
  Int ->
  WorldState ->
  m WorldState
runSteps write maybePause outputStyle world steps = go steps
  where
    go :: Int -> WorldState -> m WorldState
    go n oldState =
      do
        resultText <- execWriterT $ printWireWorld outputStyle world oldState
        write resultText
        let newState = step world oldState
        if n > 0
          then go (n - 1) newState
          else maybe (pure newState) (>> go steps newState) maybePause

runStepsWith ::
  ( MonadState WorldState m,
    MonadReader World m
  ) =>
  (Int -> m ()) ->
  Int ->
  m ()
runStepsWith interact steps = go steps
  where
    go 0 = pure ()
    go n =
      do
        interact (steps - n)
        world <- ask
        modify (step world)
        go (n - 1)
