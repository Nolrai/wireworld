{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module TextDisplay
  ( parseWireWorld,
    printWireWorld,
    onCell,
    rosettaCell,
    boxCell,
    coloredCell,
    FromCell (..),
    Parser,
    fromRows,
    parseRow,
    parseCells,
    parseMetal,
    parseSet,
    Cell (..),
    sizeToWidth,
  )
where

import Control.Monad.Writer
import Data.IntSet as IntSet hiding (size)
import Data.List as List
import Data.Vector as V hiding (mapM_)
import Data.Vector.Unboxed as UV hiding (mapM_)
import Data.WireWorld
import System.Console.Pretty
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec MyErrorData Text

data MyErrorData = Unit

instance Eq MyErrorData where
  _ == _ = True

instance Ord MyErrorData where
  _ <= _ = True

instance Semigroup MyErrorData where
  _ <> _ = Unit

instance Monoid MyErrorData where
  mempty = Unit

instance ShowErrorComponent MyErrorData where
  showErrorComponent _ = ""

data FromCell a = FromCell {emptyCell :: a, headCell :: a, tailCell :: a, metalCell :: a}

-- Only used here, in other places this data is split between the World and the WorldState.
data Cell = EmptyCell | HeadCell | TailCell | MetalCell
  deriving stock (Enum, Bounded, Ord, Eq, Show)

parseWireWorld :: FromCell Text -> Parser (World, WorldState)
parseWireWorld fc =
  do
    worldSize <- WS <$> (chunk "[" *> sepBy1 L.decimal (chunk ",") <* chunk "]" <* eol)
    fromRows fc worldSize

-- Yeah we parse the same data three times, I do not care.
fromRows :: FromCell Text -> WorldSize -> Parser (World, WorldState)
fromRows fc size =
  do
    let width = sizeToWidth size
    let (numRows :: Int, 0 :: Int) = numEntries size `divMod` width
    metal <- lookAhead $ parseMetal fc width numRows
    headCells <- lookAhead $ parseSet HeadCell fc width numRows
    tailCells <- parseSet TailCell fc width numRows
    pure (World {..}, WorldState {..})

parseMetal :: FromCell Text -> Int -> Int -> Parser (UV.Vector Bool)
parseMetal fc width numRows =
  UV.map not <$> parseCells EmptyCell fc width numRows

parseCells :: Cell -> FromCell Text -> Int -> Int -> Parser (UV.Vector Bool)
parseCells cell fc width numRows =
  V.foldr (<>) mempty <$> V.replicateM numRows (parseRow cell fc width)

parseRow :: Cell -> FromCell Text -> Int -> Parser (UV.Vector Bool)
parseRow cell FromCell {..} width =
  UV.replicateM width ((== cell) <$> parseCell) <* eol
  where
    onCell' :: Cell -> Text
    onCell' EmptyCell = emptyCell
    onCell' HeadCell = headCell
    onCell' TailCell = tailCell
    onCell' MetalCell = metalCell
    parseCellType :: Cell -> Parser Cell
    parseCellType c = chunk (onCell' c) $> c
    parseCell :: Parser Cell
    parseCell = choice $ parseCellType <$> [minBound .. maxBound]

parseSet :: Cell -> FromCell Text -> Int -> Int -> Parser IntSet
parseSet cell fc width numRows =
  toSet <$> parseCells cell fc width numRows
  where
    toSet :: UV.Vector Bool -> IntSet
    toSet =
      IntSet.fromDistinctAscList
        . UV.toList
        . UV.imapMaybe (\ix b -> if b then Just ix else Nothing)

printWireWorld :: MonadWriter Text m => FromCell Text -> World -> WorldState -> m ()
printWireWorld fc w@World {size} ws =
  do
    tell (show size)
    tell "\n"
    toRows fc w ws

-- write each row out as a line of text/string/Text
toRows :: MonadWriter Text m => FromCell Text -> World -> WorldState -> m ()
toRows fc w@World {..} ws@WorldState {..} =
  mapM_ (\row -> tell $ mconcatV row <> "\n")
    . fmap (fmap $ onCell fc w ws)
    $ rowIndices (sizeToWidth size) (UV.length metal)

sizeToWidth :: WorldSize -> Int
sizeToWidth (WS sizeList) = width
  where
    width :: Int
    (width : _) = List.reverse sizeList

-- monoid up a vector of values into a single value
mconcatV :: Monoid m => V.Vector m -> m
mconcatV = V.foldl' (<>) mempty

-- given the width and total size, poduce a list of vectors, each of whom have the indexes of one row
rowIndices :: Int -> Int -> [V.Vector Int]
rowIndices width end = go 0
  where
    go n
      | n + width < end = V.enumFromN n width : go (n + width)
      | otherwise = [V.enumFromTo n (end - 1)]

-- "prints" the cell at index ix
onCell :: FromCell a -> World -> WorldState -> Int -> a
onCell FromCell {..} World {..} WorldState {..} ix
  | not (metal UV.! ix) = emptyCell
  | ix `member` headCells = headCell
  | ix `member` tailCells = tailCell
  | otherwise = metalCell

rosettaCell :: IsString s => FromCell s
rosettaCell = FromCell "_" "H" "t" "."

boxCell :: IsString s => FromCell s
boxCell = FromCell "__" "▓▓" "▒▒" "░░"

coloredCell :: (Pretty s, IsString s) => FromCell s
coloredCell =
  FromCell
    (color Black "██")
    (color Red "██")
    (color Blue "██")
    (color Yellow "██")
