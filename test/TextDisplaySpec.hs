{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

module TextDisplaySpec
  ( spec,
    parseText,
  )
where

--import Test.QuickCheck

import Control.Arrow (left)
import Control.Monad.Writer
import Data.WireWorld (World, WorldState)
import Language.Haskell.TH.Syntax (Name)
import Test.Hspec
import Test.Utils
import Text.Megaparsec
import TextDisplay

runParserPretty :: Parser t -> FilePath -> Text -> Either String t
runParserPretty parser path input = errorBundlePretty `left` runParser parser path input

parseText :: Text -> Either String (World, WorldState)
parseText = runParserPretty (parseWireWorld rosetaCell) exampleFilePath

spec :: Spec
spec =
  beforeAll (readFileText exampleFilePath) $
    do
      describe "parseRow"
        $ it "parses an example row"
        $ const
        $ runParserPretty (parseRow EmptyCell rosetaCell 12) "test" "tH........._\r\n"
          `shouldSatisfy` isRight
      describe "parseWireWorld"
        $ it "parses the example file"
        $ \text -> parseText text `shouldSatisfy` isRight
      describe
        (show 'printWireWorld)
        printWireWorldTests

printWireWorldTests :: SpecWith Text
printWireWorldTests =
  [(rosetaCell, 'rosetaCell), (boxCell, 'boxCell), (coloredCell, 'coloredCell)]
    `forM_` uncurry toTest
  where
    toTest :: FromCell Text -> Name -> SpecWith Text
    toTest fc fcname =
      it (show fcname) $
        \text ->
          case parseText text of
            Left err -> error (toText err)
            Right (w, ws) ->
              goldenTextTest
                (show 'printWireWorld <> "." <> show fcname <> ".ww")
                (execWriter $ printWireWorld fc w ws)
