{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wall #-}

module Main where

import Data.Functor.Rep
import GHC.Exts (IsList(..))
import NumHask.Array
import NumHask.Hedgehog
import NumHask.Prelude as P
import Numeric.Dimensions as D
import Test.DocTest
import qualified Hedgehog as H
import qualified NumHask.Hedgehog.Prop.Space as I
import qualified Prelude

genAIntegral :: forall a m r. (H.MonadGen m, Dimensions r, Additive a, Bounded a, ToInteger a, FromInteger a) => m (Array [] r a)
genAIntegral = fromList <$> replicateM n integral_
  where
    n = dimVal $ dim @r

genARational :: forall a m r. (H.MonadGen m, Dimensions r, Field a, Subtractive a, ToRatio a, FromRatio a) => m (Array [] r a)
genARational = fromList <$> replicateM n negUniform
  where
    n = dimVal $ dim @r

main :: IO ()
main = do
  putStrLn ("Array DocTest turned on" :: Text)
  doctest ["src/NumHask/Array.hs"]
  putStrLn ("Example DocTest turned on" :: Text)
  doctest ["src/NumHask/Array/Example.hs"]
  bVInt <- assertProps "Vector Int 6" (Prelude.fromInteger 100)
    (genAIntegral :: H.Gen (Vector [] 6 Int)) integralProps'
  bMInt <- assertProps "Matrix [] '[3,4] Int" (Prelude.fromInteger 100)
    (genAIntegral :: H.Gen (Array [] '[3,4] Int)) integralProps'
  -- bVFloat <- assertProps "Vector Float 6" (Prelude.fromInteger 100)
  --  (genARational :: H.Gen (Vector [] 6 Float)) (fieldProps' acc)
  bMFloat <- assertProps "Array [] '[3,4] Float" (Prelude.fromInteger 100)
    (genARational :: H.Gen (Array [] '[3,4] Float)) (fieldProps' acc)
  unless (bVInt && bMInt && bMFloat)
    exitFailure
  where
    acc = tabulate (const 1.0)

integralProps'
  :: forall a.
  ( Show a
  , Eq a
  , Distributive a
  , Subtractive a
  , Multiplicative a
  , Signed a
  )
  => H.Gen a
  -> [(H.PropertyName, H.Property)]
integralProps' g = mconcat $
  (\x -> x g) <$>
  [ isAdditive
  , isSubtractive
  , isMultiplicative
  , \x -> [("distributive", isDistributive zero (+) (*) x)]
  , \x -> [("signed", NumHask.Hedgehog.isSigned x)]
  ]

-- | field laws
fieldProps'
  :: forall a.
  ( Show a
  , Epsilon a
  , Lattice a
  , LowerBoundedField a
  , UpperBoundedField a
  , BoundedJoinSemiLattice a
  , BoundedMeetSemiLattice a
  , Signed a
  )
  => a
  -> H.Gen a
  -> [(H.PropertyName, H.Property)]
fieldProps' acc g = mconcat $
  (\x -> x g) <$>
  [ I.isAdditive acc
  , \x -> [("subtractive", I.isSubtractive acc x)]
  , I.isMultiplicative acc
  , \x -> [("distributive", I.isDistributiveTimesPlus one x)]
  , \x -> [("divisive", I.isDivisive one x)]
  , \x -> [("signed", I.isSigned one x)]
  ]
