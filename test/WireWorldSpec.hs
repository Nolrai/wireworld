{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module WireWorldSpec (spec) where

import Test.Hspec
import Test.Hspec.SmallCheck as HSC
import Test.SmallCheck.Series (Serial(..), newtypeCons, getNonNegative)
import Test.SmallCheck ((==>))


import WireWorld (WorldSize(..), neighborIndexes, dimension)

instance (Monad m) => Serial m WorldSize where
  series = newtypeCons (WS . ((+2) . getNonNegative <$>))

spec :: Spec
spec = do
  describe "neighborIndexes" $ do
    it "produces valid indexes" $ HSC.property $
      \ sizeList x ->
        all (\i -> i >= 0 && i < product (unWS sizeList))
          $ neighborIndexes sizeList x
    xit "produces 3^dim - 1 values" $ HSC.property $
      \ sizeList x ->
        (all (>=3) . unWS) sizeList ==> length (neighborIndexes sizeList x) `shouldBe` (3 ^ dimension sizeList) - 1