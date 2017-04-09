{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ConstraintKinds #-}
{-# OPTIONS_GHC -Wall #-}

-- | Field
module NumHask.Algebra.Field (
    Field
  ) where

import Protolude (Double, Float)
import NumHask.Algebra.Additive
import NumHask.Algebra.Multiplicative
import NumHask.Algebra.Distribution
import NumHask.Algebra.Ring

-- | Field
class ( AdditiveGroup a
      , MultiplicativeGroup a
      , Distribution a
      , Ring a) =>
      Field a

instance Field Double
instance Field Float
