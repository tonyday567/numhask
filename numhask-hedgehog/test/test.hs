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

tests :: H.TestLimit -> IO Bool
tests n = do
  bInt <- assertProps "Int" n
    (integral_ :: H.Gen Int) integralProps
  bInt8 <- assertProps "Int8" n
    (integral_ :: H.Gen Int8) integralProps
  bWord8 <- assertProps "Word8" n
    (integral_ :: H.Gen Word8)
    integralProps
  bInteger <- assertProps "Integer" n
    (integral (Range.constantFrom zero -1000000 1000000) :: H.Gen Integer)
    integralUnboundedProps
  bNatural <- assertProps "Natural" n
    (integral (Range.constantFrom zero zero 1000000) :: H.Gen Natural)
    naturalProps
  bBool <- assertProps "Bool" n Gen.bool
   boolProps
  bRational <- assertProps "Rational" n
    (negUniform :: H.Gen Rational) rationalProps
  bFloat <- assertProps "Float" n
    (negUniform :: H.Gen Float) fieldProps
  bQuotientFloat <- assertProps "Float - Quotient" n
    (negUniform :: H.Gen Float) quotientFieldProps
  bComplexFloat <- assertProps "Complex Float" n
    (genComplex (negUniform :: H.Gen Float))
    (complexFieldProps (5.0 :+ 5.0))
  bPairFloat <- assertProps "Pair Float" n
    (genPair (negUniform :: H.Gen Float)) fieldProps
  bLatticeFloat <- assertProps "Float Lattice" n
    (negUniform :: H.Gen Float) latticeProps
  bLatticeComplex <- assertProps "Complex Lattice" n
    (genComplex (negUniform :: H.Gen Float)) latticeProps
  bSpaceFloat <- assertProps "Space Float" n
    (genIntervalFloat (negUniform :: H.Gen Float)) spaceProps

  return $ bInt && bInt8 && bWord8 && bInteger && bNatural && bBool &&
    bRational && bFloat && bComplexFloat && bLatticeFloat && bLatticeComplex && bSpaceFloat
    && bPairFloat && bQuotientFloat

main :: IO ()
main = do
  ok <- tests (P.fromInteger 100 :: H.TestLimit)
  unless ok
    exitFailure
