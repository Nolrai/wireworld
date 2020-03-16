{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE TypeApplications #-}

module Genetic
  ( runWithTestInputs,
    genetic,
    scoreGenome,
    scoreResult,
    expectedOutput,
    mkWorld,
    unMkWorld,
    initialize,
  )
where

import Control.Lens (view)
import Control.Monad.RWS
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
  Int ->
  Int ->
  Int ->
  World ->
  WorldState ->
  [Vector Bool]
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

scoreResult :: [Vector Bool] -> [Vector Bool] -> V2 Int
scoreResult expected output = fmap (+ 1) (V2 wrongness delay)
  where
    wrongness, delay :: Int
    wrongness =
      hammingDistance
        (trimedOutput <> List.repeat mempty)
        trimedExpected
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
  V2 Int
scoreGenome op period dataWidth maxDelay ws genome =
  let opt f = f period dataWidth
      expected = opt expectedOutput op
      result =
        opt runWithTestInputs maxDelay (mkWorld ws genome) mempty
   in scoreResult expected result

mkWorld :: WorldSize -> Genome Bool -> World
mkWorld ws genome =
  World {size = ws, metal = UV.fromList $ addEdges genome}
  where
    addEdges :: Genome Bool -> Genome Bool
    addEdges g =
      intercalate [False] (splitEvery (sizeToWidth ws - 1) g)
        <> [False] -- intercalate doesn't put a [False] at the end of the last row.
        <> List.replicate (sizeToWidth ws) False

unMkWorld :: World -> Genome Bool
unMkWorld World {size, metal} = removeEdges (UV.toList metal)
  where
    removeEdges =
      List.concat
        . List.init
        . fmap List.init
        . splitEvery (sizeToWidth size)

toMultiObjectiveProblem ::
  (Vector Bool -> Vector Bool) ->
  Int ->
  Int ->
  Int ->
  WorldSize ->
  MultiObjectiveProblem (Genome Bool -> Double)
toMultiObjectiveProblem op period dataWidth maxDelay ws =
  [ (Minimizing, fromIntegral . wrongness),
    (Minimizing, fromIntegral . delay)
  ]
  where
    wrongness, delay :: Genome Bool -> Int
    wrongness = view _x . scoreFunction
    delay = view _y . scoreFunction
    scoreFunction :: Genome Bool -> V2 Int
    scoreFunction = scoreGenome op period dataWidth maxDelay ws

geneticStep ::
  (Vector Bool -> Vector Bool) ->
  Int ->
  Int ->
  Int ->
  WorldSize ->
  StepGA Rand Bool
geneticStep op period dataWidth maxDelay ws =
  stepNSGA2bt
    (toMultiObjectiveProblem op period dataWidth maxDelay ws)
    (onePointCrossover 0.1)
    mutate
  where
    mutate =
      constFrequencyMutate @Double 2.0 >=> asymmetricMutate 0.02 0.05

orStep :: StepGA Rand Bool
orStep = applyDefault geneticStep

applyDefault ::
  ( (Vector Bool -> Vector Bool) ->
    Int ->
    Int ->
    Int ->
    WorldSize ->
    a
  ) ->
  a
applyDefault f = f (UV.singleton . UV.or) 5 3 10 boardSize

boardSize :: WorldSize
boardSize = WS [5, 5]

initialize :: Int -> Rand [Genome Bool]
initialize popSize =
  getRandomBinaryGenomes popSize $
    numEntries smallerSize
  where
    smallerSize = WS . fmap (\x -> x - 1) . unWS $ boardSize

genetic :: Int -> Int -> IO [(World, [Objective])]
genetic generations popSize =
  do
    result <- runGA (initialize popSize) $ loop (Generations generations) orStep
    pure $ first (mkWorld boardSize) <$> reScore (fst <$> result)
  where
    reScore :: [Genome Bool] -> [(Genome Bool, [Objective])]
    reScore = evalAllObjectives problems
    problems :: MultiObjectiveProblem (Genome Bool -> Double)
    problems = applyDefault toMultiObjectiveProblem
