{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
-- | Conventions defining the Natural Numbers
module NumHask.Algebra.Natural
    (
        Natural
    )
where
import           NumHask.Algebra.Abstract.Group
import           NumHask.Algebra.Abstract.Ring
import           NumHask.Algebra.Abstract.Multiplicative
import           NumHask.Algebra.Abstract.Additive
import           NumHask.Algebra.Integral
import qualified          Numeric.Natural as DN

class (Addition a, Unital (Sum a), Multiplication a, Unital (Product a), Distributive  a, FromInteger a) => Natural a

instance Natural DN.Natural

--FIXME: Bad?
instance Natural Int