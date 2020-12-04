{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

module Main
  ( main,
  )
where

import Gauge.Main
import Text.Megaparsec as MP
import WireWorld

exampleFilePath :: FilePath
exampleFilePath = "./data/example.ww"

deriving anyclass instance NFData WorldState

deriving stock instance Generic WorldState

main :: IO ()
main =
  do
    fileText <- readFileText exampleFilePath
    (world, start) <-
      either
        (die . errorBundlePretty)
        pure
        $ MP.runParser (parseWireWorld rosettaCell) exampleFilePath fileText
    defaultMain [bench (show 'runSteps) (nfAppIO (runSteps (const . pure $ ()) Nothing rosettaCell world 1000) start)]
