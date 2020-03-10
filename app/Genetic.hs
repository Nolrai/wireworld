{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Genetic where
import Control.Monad.RWS
import Control.Monad.Writer
import Data.IntSet as IntSet hiding (size)
import Data.Vector.Unboxed as UV hiding (modify)
import WireWorld
import Linear.V2
import Moo.GeneticAlgorithm.Binary
import TextDisplay

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
  Monoid (a (Vector Bool)),
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
testInput :: Int -> Int -> Int -> [Bool]
testInput period dataWidth step =
UV.fromList $ encodeBinary (0, 2 ^ dataWidth - 1) inputAsInt
where
  (d, m) = step `divMod` period
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
readWriteTestInput period dataWidth step =
do
  readOutputs
  writeInputs (testInput step)

runWithTestInputs :: Int -> Int -> World -> WorldState -> [Vector Bool]
runWithTestInputs period dataWidth =
snd . evalRWS
  . runStepsWith (readWriteTestInput period dataWidth)
  $ period * 2 ^ dataWidth
