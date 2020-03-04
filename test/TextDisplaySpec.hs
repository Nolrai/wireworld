{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module TextDisplaySpec
  ( spec,
  )
where

--import Test.QuickCheck

import Control.Arrow (left)
import Test.Hspec
import Text.Megaparsec
import TextDisplay

exampleFilePath :: FilePath
exampleFilePath = "./data/example.ww"

runParserPretty :: Parser t -> FilePath -> Text -> Either String t
runParserPretty parser path input = errorBundlePretty `left` runParser parser path input

spec :: Spec
spec =
  do
    describe "parseWireWorld" $ do
      it "parses the example file" $ do
        text <- readFileText exampleFilePath
        runParserPretty (parseWireWorld rosetaCell) exampleFilePath text
          `shouldSatisfy` isRight
