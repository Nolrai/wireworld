{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Options
  ( execOptions,
    Options (..),
  )
where

import Data.List as List
import Data.Text as Text
import Options.Applicative as O
import TextDisplay as TD

data Options where
  RunCommand :: {file :: FilePath, steps :: Int, inputStyle :: FromCell Text, outputStyle :: FromCell Text, exit :: Maybe (IO a)} -> Options

execOptions = execParser $ parseOptions `info` myInfo

myInfo = briefDesc <> progDesc "run a wireworld simulation." <> failureCode (-1)

parseOptions :: O.Parser Options
parseOptions =
  subparser
    ( command
        "run"
        (runCommand `info` progDesc "load a ww file and run steps")
    )

runCommand :: O.Parser Options
runCommand =
  RunCommand <$> fileParser <*> stepsParser <*> inputStyleParser <*> outputStyleParser <*> exitParser

fileParser =
  strOption (short 'f' <> long "file" <> metavar "FILEPATH" <> help "The wireworld file to read in" <> action "file")

stepsParser :: O.Parser Int
stepsParser =
  option
    auto
    (short 'n' <> long "steps" <> metavar "INT" <> help "the number of steps of simulation to run before exiting/waiting")

styleParser :: Char -> O.Parser (FromCell Text)
styleParser c = List.foldr (\(f, name) others -> others <|> flag' f (long $ [c] <> name)) (customStyleParser c) styles

inputStyleParser :: O.Parser (FromCell Text)
inputStyleParser = styleParser 'i'

outputStyleParser :: O.Parser (FromCell Text)
outputStyleParser = styleParser 'o'

styles :: [(FromCell Text, String)]
styles = [(rosetaCell, "roseta"), (boxCell, "box"), (coloredCell, "color")]

customStyleParser c = option readCustomStyle (short 'c' <> long "custom")

readCustomStyle :: ReadM (FromCell Text)
readCustomStyle = maybeReader $
  \(str :: String) ->
    do
      let text = toText str
      let [emptyCell, headCell, tailCell, metalCell] = chop (Text.length text `div` 4) text
      pure FromCell {..}

chop chunkSize string =
  let (here, there) = Text.splitAt chunkSize string
   in here : chop chunkSize there

exitParser = flag (Just getLine) Nothing (short 'e' <> long "exit")
