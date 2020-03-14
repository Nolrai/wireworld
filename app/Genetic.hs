{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Genetic
  ( runWithTestInputs,
    genetic,
  )
where

import Control.Monad.RWS
import Control.Monad.Writer
import Data.IntSet as IntSet hiding (size)
import Data.List as List hiding (union)
import Data.Vector.Unboxed as UV hiding (modify)
import Linear.V2
import Moo.GeneticAlgorithm.Binary
import Moo.GeneticAlgorithm.Multiobjective
import TextDisplay
import WireWorld

writeInputs ::
  (MonadState WorldState m, MonadReader World m) =>
  Vector Bool ->
  m ()
writeInputs input =
  do
    World {size, metal} <- ask
    old <- get
    let (firstRow :: Vector Bool) = UV.take (sizeToWidth size) metal
    let metalIndexVector = UV.imapMaybe (flip $ ($>) . guard) firstRow
    let inputIndexVectors =
          UV.partition fst $
            UV.zip input metalIndexVector
    let (V2 onesIndexSet zerosIndexSet) :: V2 IntSet =
          IntSet.fromAscList . UV.toList . UV.map snd
            <$> uncurry V2 inputIndexVectors
    put
      ( old
          { headCells =
              (headCells old `union` onesIndexSet)
                `difference` zerosIndexSet
          }
      )

readOutputs ::
  ( Applicative a,
    MonadWriter (a (Vector Bool)) m,
    MonadReader World m,
    MonadState WorldState m
  ) =>
  m (Vector Bool)
readOutputs =
  do
    World {size, metal} <- ask
    WorldState {headCells} <- get
    let indexedMetal = UV.imap (,) metal
    let width = sizeToWidth size
    let secondToLastRow :: Vector (Int, Bool) =
          UV.drop width . UV.take (2 * width) $ UV.reverse indexedMetal
    let onlyIf (ix, b) = guard b $> ix
    let reversedIndexVector = UV.mapMaybe onlyIf secondToLastRow
    let indexVector = UV.reverse reversedIndexVector
    let outputVector = UV.map (`IntSet.member` headCells) indexVector
    tell (pure outputVector)
    pure outputVector

-- test input for a logic gate
testInput :: Int -> Int -> Int -> Vector Bool
testInput period dataWidth stepNumber =
  UV.fromList $ encodeBinary (0, 2 ^ dataWidth - 1) inputAsInt
  where
    (d, m) = stepNumber `divMod` period
    inputAsInt = if m == 0 then d else 0 -- space/time between eletrons

readWriteTestInput ::
  forall (m :: Type -> Type).
  ( MonadWriter [Vector Bool] m,
    MonadReader World m,
    MonadState WorldState m
  ) =>
  Int ->
  Int ->
  Int ->
  m ()
readWriteTestInput period dataWidth stepNumber =
  readOutputs >> writeInputs (testInput period dataWidth stepNumber)

runWithTestInputs ::
  Int -> Int -> Int -> World -> WorldState -> [Vector Bool]
runWithTestInputs period dataWidth maxDelay world worldState =
  snd $
    evalRWS
      ( runStepsWith
          (readWriteTestInput period dataWidth)
          (period * 2 ^ dataWidth + maxDelay)
      )
      world
      worldState

expectedOutput ::
  Int ->
  Int ->
  (Vector Bool -> Vector Bool) ->
  [Vector Bool]
expectedOutput period dataWidth op =
  op . testInput period dataWidth
    <$> (UV.toList . UV.enumFromN 0 $ period * 2 ^ dataWidth)

scoreResult :: [Vector Bool] -> [Vector Bool] -> (Int, Int)
scoreResult expected output = (wrongness, delay)
  where
    wrongness = hammingDistance trimedOutput trimedExpected
    delay = List.length cutOutput - List.length cutExpected
    (cutExpected, trimedExpected) = List.span (UV.all (== False)) expected
    (cutOutput, trimedOutput) = List.span (UV.all (== False)) output

scoreGenome ::
  (Vector Bool -> Vector Bool) ->
  Int ->
  Int ->
  Int ->
  WorldSize ->
  Genome Bool ->
  (Int, Int)
scoreGenome op period dataWidth maxDelay ws genome =
  scoreResult
    ( expectedOutput
        period
        dataWidth
        op
    )
    ( runWithTestInputs
        period
        dataWidth
        maxDelay
        (World {size = ws, metal = UV.fromList genome})
        mempty
    )

geneticStep ::
  (Vector Bool -> Vector Bool) ->
  Int ->
  Int ->
  Int ->
  WorldSize ->
  StepGA Rand Bool
geneticStep op period dataWidth maxDelay ws =
  stepNSGA2bt
    ( [(Minimizing, fromIntegral . wrongness), (Minimizing, fromIntegral . delay)] ::
        MultiObjectiveProblem (Genome Bool -> Double)
    )
    (onePointCrossover 0.1)
    (pointMutate 0.1)
  where
    wrongness, delay :: Genome Bool -> Int
    wrongness = fst . scoreFunction
    delay = snd . scoreFunction
    scoreFunction :: Genome Bool -> (Int, Int)
    scoreFunction = scoreGenome op period dataWidth maxDelay ws

orStep :: StepGA Rand Bool
orStep =
  geneticStep (UV.singleton . UV.or) 5 3 20 boardSize

boardSize :: WorldSize
boardSize = WS [10, 10]

initialize :: Int -> Rand [Genome Bool]
initialize popSize = getRandomBinaryGenomes popSize (numEntries boardSize)

genetic :: Int -> Int -> IO ()
genetic generations popSize =
  do
    result <-
      runGA (initialize popSize) $
        loop (Generations generations) orStep
    let (winner, score) : _ = result
    print score
    text <-
      execWriterT $
        printWireWorld
          coloredCell
          (World {size = boardSize, metal = UV.fromList winner})
          mempty
    putTextLn text
