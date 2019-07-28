{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RebindableSyntax #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main where

import NumHask.Hedgehog
import NumHask.Prelude
import qualified Hedgehog as H
import qualified Hedgehog.Internal.Gen as Gen
import qualified Hedgehog.Range as Range
import qualified Prelude as P

asserts :: H.TestLimit -> [IO Bool]
asserts n =
  [ assertProps "Int" n (integral_ :: H.Gen Int) integralProps
  , assertProps "Int8" n
    (integral_ :: H.Gen Int8) integralProps
  , assertProps "Word8" n
    (integral_ :: H.Gen Word8)
    integralProps
  , assertProps "Integer" n
    (integral (Range.constantFrom zero -1000000 1000000) :: H.Gen Integer)
    integralUnboundedProps
  , assertProps "Natural" n
    (integral (Range.constantFrom zero zero 1000000) :: H.Gen Natural)
    naturalProps
  , assertProps "Bool" n Gen.bool
    boolProps
  , assertProps "Rational" n
    (negUniform :: H.Gen (Ratio Integer)) rationalProps
  , assertProps "Float" n
    (negUniform :: H.Gen Float) fieldProps
  , assertProps "Float - Quotient" n
    (negUniform :: H.Gen Float) quotientFieldProps
  , assertProps "Complex Float" n
    (genComplex (negUniform :: H.Gen Float))
    (complexFieldProps (5.0 :+ 5.0))
  , assertProps "Pair Float" n
    (genPair (negUniform :: H.Gen Float)) fieldProps
  , assertProps "Float Lattice" n
    (negUniform :: H.Gen Float) latticeProps
  , assertProps "Complex Lattice" n
    (genComplex (negUniform :: H.Gen Float)) latticeProps
  , assertProps "Space Properties" n
    (genRange (negUniform :: H.Gen Float)) spaceProps
  , assertProps "FieldSpace" n
    (genRange (negUniform :: H.Gen Float)) fieldSpaceProps
  , assertProps "Space Algebra" n
    (genRangePos (negUniform :: H.Gen Float))
    spaceAlgebraProps
  ]

main :: IO ()
main = do
  ok <- all P.id <$> sequence (asserts (P.fromInteger 100 :: H.TestLimit))
  unless ok
    exitFailure
