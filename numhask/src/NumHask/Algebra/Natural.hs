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
import           NumHask.Algebra.Abstract.Multiplication
import           NumHask.Algebra.Abstract.Addition
import           NumHask.Algebra.Integral
import qualified          Numeric.Natural as DN

class (Addition a, Unital (Add a), Multiplication a, Unital (Mult a), Distribution a, FromInteger a) => Natural a

instance Natural DN.Natural

--FIXME: Bad?
instance Natural Int