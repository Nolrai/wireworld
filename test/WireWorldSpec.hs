{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

module WireWorldSpec
  ( spec,
  )
where

import Control.Monad.Writer
import Test.Hspec
import Test.Utils
import TextDisplaySpec hiding (spec)
import WireWorld

exampleSteps :: Int
exampleSteps = 100

spec :: Spec
spec =
  do
    parseResult <- runIO (parseText <$> readFileText exampleFilePath)
    describe "runSteps" $
      case parseResult of
        Right (w, ws) ->
          it "produces example output" $
            goldenTextTest
              (show 'runSteps <> "_" <> show exampleSteps <> ".ww")
              (execWriter $ runSteps (tell . (<> "\n")) Nothing rosetaCell w exampleSteps ws)
        Left err -> it "produces example output" . pendingWith $ "example failed to parse: " <> err
