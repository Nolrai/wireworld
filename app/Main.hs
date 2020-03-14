{-# LANGUAGE RecordWildCards #-}

module Main
  ( main,
  )
where

import Data.Vector.Unboxed as UV hiding (mapM_)
import Genetic
import Options (ECO (..), Options (..), RCO (..), execOptions)
import Text.Megaparsec as MP
import TextDisplay
import WireWorld

main :: IO ()
main = execOptions >>= body

body :: Options -> IO ()
body (Run RCO {..}) =
  withWWFile rco_file rco_inputStyle $
    \(world, start) ->
      do
        _ <- runSteps putTextLn rco_exit rco_outputStyle world rco_steps start
        pure ()
body (Eval ECO {..}) =
  withWWFile eco_file eco_inputStyle $
    \(world, start) ->
      do
        printVector `mapM_` mkOutput world start
        pure ()
  where
    printVector :: Vector Bool -> IO ()
    printVector = print . UV.map (if' (1 :: Int) 0)
    mkOutput :: World -> WorldState -> [Vector Bool]
    mkOutput = runWithTestInputs eco_period eco_dataWidth eco_maxDelay
    if' :: a -> a -> Bool -> a
    if' t e b = if b then t else e

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
