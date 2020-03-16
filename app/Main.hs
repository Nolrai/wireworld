{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main
  ( main,
  )
where

import Control.Monad.Writer
import qualified Data.Vector as V
import Data.Vector.Unboxed as UV hiding (mapM_)
import Genetic
import Linear.V2
import Main.Utf8 (withUtf8)
import Options (ECO (..), GCO (..), Options (..), RCO (..), execOptions)
import Text.Megaparsec as MP
import TextDisplay
import WireWorld

main :: HasCallStack => IO ()
main = withUtf8 (execOptions >>= body)

body :: Options -> IO ()
body (Run RCO {..}) =
  withWWFile rco_file rco_inputStyle $
    \(world, start) ->
      do
        _ <- runSteps putTextLn rco_exit rco_outputStyle world rco_steps start
        pure ()
body (Eval ECO {..}) =
  withWWFile eco_file eco_inputStyle $
    \(world@World {..}, start) ->
      do
        print $ toIntVector <$> mkOutput world start
        print $ toIntVector <$> mkExpected
        print $ mkScore size (unMkWorld world)
  where
    mkOutput :: World -> WorldState -> [Vector Bool]
    mkOutput = runWithTestInputs eco_period eco_dataWidth eco_maxDelay
    mkExpected = expectedOutput eco_period eco_dataWidth op
    op :: Vector Bool -> Vector Bool
    op = singleton . UV.or
    mkScore :: WorldSize -> [Bool] -> V2 Int
    mkScore = scoreGenome op eco_period eco_dataWidth eco_maxDelay
body (Evolve GCO {..}) =
  do
    results <-
      V.fromList <$> genetic gco_size gco_generations
    results
      `iForM_` \ix (world, score) ->
        do
          putTextLn $
            "Victor " <> show ix <> " has score " <> show score
          putTextLn . execWriter $
            printWireWorld gco_outputStyle world mempty
          putTextLn ""
  where
    iForM_ = flip V.imapM_

withWWFile ::
  (MonadIO m) =>
  FilePath ->
  FromCell Text ->
  ((World, WorldState) -> m ()) ->
  m ()
withWWFile file inputStyle action =
  do
    fileText <- readFileText file
    either
      (die . errorBundlePretty)
      action
      $ MP.runParser (parseWireWorld inputStyle) file fileText
