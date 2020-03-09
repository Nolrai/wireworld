{-# LANGUAGE NamedFieldPuns #-}

module Main
  ( main,
  )
where

import Options (Options (..), execOptions)
import Text.Megaparsec as MP
import TextDisplay
import WireWorld

main :: IO ()
main = execOptions >>= body >> pure ()

body :: Options -> IO WorldState
body RunCommand {file, steps, inputStyle, outputStyle, exit} =
  do
    fileText <- readFileText file
    either
      (die . errorBundlePretty)
      (\(world, start) -> runSteps putTextLn exit outputStyle world steps start)
      $ MP.runParser (parseWireWorld inputStyle) file fileText
