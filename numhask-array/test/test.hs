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
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Main where

-- import Data.Functor.Rep
import GHC.Exts (IsList(..))
import NumHask.Array.Fixed
import NumHask.Array.Shape
import NumHask.Hedgehog
import NumHask.Prelude as P
import Test.DocTest
import qualified Hedgehog as H
import qualified NumHask.Hedgehog.Prop.Space as I
-- import qualified Prelude

genAIntegral :: forall a m r. (HasShape r, H.MonadGen m, Additive a, Bounded a, ToInteger a, FromInteger a) => m (Array (r :: [Nat]) a)
genAIntegral = fromList <$> replicateM (fromIntegral n) integral_
  where
    n = product $ shapeVal $ toShape @r

genARational :: forall a m r. (Ord a, H.MonadGen m, HasShape r, Field a, Subtractive a, ToRatio a Integer, FromRatio a Integer) => m (Array (r :: [Nat]) a)
genARational = fromList <$> replicateM (fromIntegral n) negUniform
  where
    n = product $ shapeVal $ toShape @r

main :: IO ()
main = do
  putStrLn ("NumHask.Array.Fixed DocTest turned on" :: Text)
  doctest ["src/NumHask/Array/Fixed.hs"]
  putStrLn ("NumHask.Array.Shape DocTest turned on" :: Text)
  doctest ["src/NumHask/Array/Shape.hs"]
  {-
  bVInt <- assertProps "Vector Int 6" (Prelude.fromInteger 100)
    (genAIntegral :: H.Gen (Vector 6 Int)) integralProps'
  bMInt <- assertProps "Matrix '[3,4] Int" (Prelude.fromInteger 100)
    (genAIntegral :: H.Gen (Array '[3,4] Int)) integralProps'
  bVFloat <- assertProps "Vector Float 6" (Prelude.fromInteger 100)
    (genARational :: H.Gen (Vector 6 Float)) (fieldProps' acc6)
  bMFloat <- assertProps "Array '[3,4] Float" (Prelude.fromInteger 100)
    (genARational :: H.Gen (Array '[3,4] Float)) (fieldProps' acc34)
  unless (bVInt && bMInt && bVFloat && bMFloat)
    exitFailure
    where
      acc6 = tabulate (const 1.0)
      acc34 = tabulate (const 1.0)
-}

integralProps'
  :: forall a.
  ( Show a
  , Eq a
  , Distributive a
  , Subtractive a
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
  , BoundedJoinSemiLattice a
  , BoundedMeetSemiLattice a
  , Ord a
  , Fractional a
  , LowerBoundedField a
  , UpperBoundedField a
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
