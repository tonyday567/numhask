{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}

module NumHask.Hedgehog.Gen
  ( rational
  , rational_
  , integral
  , integral_
  , uniform
  , negUniform
  , genPair
  , genRange
  , genRangePos
  , genComplex
  ) where

import Hedgehog as H
import NumHask.Prelude as P
import qualified Hedgehog.Internal.Gen as Gen
import qualified Hedgehog.Internal.Seed as Seed
import qualified Hedgehog.Range as Range

-- * hedgehog rng's are Num instances, so we supply a few of our own
-- There are basically two types of random variates: a discrete Integer type and a continuous rational type

-- | a rational-style random variate
rational :: (ToRatio a Integer, FromRatio a Integer, MonadGen m) => Range.Range a -> m a
rational r =
  Gen.generate $ \size seed ->
    let
      (x, y) =
        Range.bounds size r
    in
      fromRatio . (toRatio :: Double -> Ratio Integer) . fst $
        Seed.nextDouble (fromRatio $ (toRatio x :: Ratio Integer)) (fromRatio $ (toRatio y :: Ratio Integer)) seed


-- | an integral-type random variate
-- integral :: (ToIntegral a Integer, FromIntegral a Integer, MonadGen m) => Range.Range a -> m a
integral
  :: (MonadGen m, FromInteger a, ToInteger a)
  => Range.Range a -> m a
integral r =
  Gen.generate $ \size seed ->
    let
      (x, y) =
        Range.bounds size r
    in
      fromIntegral . fst $
        Seed.nextInteger (toInteger x) (toInteger y) seed

-- | an integral-style random variate utilising Bounds
integral_ ::
  ( Additive a
  , Bounded a
  , ToInteger a
  , FromInteger a
  , MonadGen m)
  => m a
integral_ = integral (Range.constantFrom zero minBound maxBound)

-- | a rational style random variate utilising Bounds
rational_ ::
  ( Additive a
  , Bounded a
  , ToRatio a Integer
  , FromRatio a Integer
  , MonadGen m)
  => m a
rational_ = rational (Range.constantFrom zero minBound maxBound)

-- | a uniform distribution between zero and one
uniform ::
  ( Field a
  , ToRatio a Integer
  , FromRatio a Integer
  , MonadGen m)
  => m a
uniform = rational (Range.constantFrom zero zero one)

-- | a uniform distribution between -1 and 1
negUniform ::
  ( Field a
  , ToRatio a Integer
  , FromRatio a Integer
  , Subtractive a
  , MonadGen m)
  => m a
negUniform = rational (Range.constantFrom zero (negate one) one)

-- | a complex random variate
genComplex :: Monad m => m a -> m (Complex a)
genComplex g = do
  r <- g
  i <- g
  pure (r :+ i)

-- | Space
genRange :: forall a m. (JoinSemiLattice a, MeetSemiLattice a, MonadGen m) => m a -> m (P.Range a)
genRange g = do
  a <- g
  b <- g
  pure (a >.< b)

genRangePos :: forall a m. (JoinSemiLattice a, MeetSemiLattice a, MonadGen m) => m a -> m (P.Range a)
genRangePos g = do
  a <- g
  b <- g
  pure (a ... b)

-- | a pair
genPair :: (Monad m) => m a -> m (Pair a)
genPair g = do
  a <- g
  b <- g
  pure (Pair a b)
