{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE RebindableSyntax #-}

module NumHask.Hedgehog.Gen where

import Hedgehog as H
import NumHask.Prelude
import qualified Hedgehog.Internal.Gen as Gen
import qualified Hedgehog.Internal.Seed as Seed
import qualified Hedgehog.Range as Range

-- * hedgehog rng's are Num instances, so we supply a few of our own
-- There are basically two types of random variates: a discrete Integer type and a continuous rational type

-- | a rational-style random variate
rational :: (ToRatio a, FromRatio a, MonadGen m) => Range a -> m a
rational range =
  Gen.generate $ \size seed ->
    let
      (x, y) =
        Range.bounds size range
    in
      fromRational . fst $
        Seed.nextDouble (fromRational x) (fromRational y) seed

-- | an integral-stype random variate
integral :: (ToInteger a, FromInteger a, MonadGen m) => Range a -> m a
integral range =
  Gen.generate $ \size seed ->
    let
      (x, y) =
        Range.bounds size range
    in
      fromIntegral . fst $
        Seed.nextInteger (fromIntegral x) (fromIntegral y) seed

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
  , ToRatio a
  , FromRatio a
  , MonadGen m)
  => m a
rational_ = rational (Range.constantFrom zero minBound maxBound)

-- | a uniform distribution between zero and one
uniform ::
  ( Field a
  , ToRatio a
  , FromRatio a
  , MonadGen m)
  => m a
uniform = rational (Range.constantFrom zero zero one)

-- | a uniform distribution between -1 and 1
negUniform ::
  ( Field a
  , ToRatio a
  , FromRatio a
  , MonadGen m)
  => m a
negUniform = rational (Range.constantFrom zero (negate one) one)

-- | a complex random variate
genComplex :: Monad m => m a -> m (Complex a)
genComplex g = do
  r <- g
  i <- g
  pure (r :+ i)

