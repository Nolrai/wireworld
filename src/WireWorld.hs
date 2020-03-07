{-# LANGUAGE ScopedTypeVariables #-}

module WireWorld
  ( module Data.WireWorld,
    module TextDisplay,
    runSteps,
  )
where

import Control.Monad.Writer
import Data.WireWorld
import TextDisplay

runSteps :: forall m t. Monad m => (Text -> m ()) -> Maybe (m t) -> FromCell Text -> World -> Int -> WorldState -> m ()
runSteps write maybePause outputStyle world steps = go steps :: WorldState -> m ()
  where
    go :: Int -> WorldState -> m ()
    go n oldState =
      do
        resultText <- execWriterT $ printWireWorld outputStyle world oldState
        write resultText
        let newState = step world oldState
        if n > 0
          then go (n - 1) newState
          else maybe (pure ()) (>> go steps newState) maybePause
