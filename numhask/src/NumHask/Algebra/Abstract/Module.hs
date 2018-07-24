{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wall #-}

-- | Algebra for Modules
module NumHask.Algebra.Abstract.Module
  ( Module
  ) where

import NumHask.Algebra.Abstract.Additive
import NumHask.Algebra.Abstract.Group
import NumHask.Algebra.Abstract.Ring

-- | a module
--   A Module over r a is a (Ring a), an abelian (Group r a) 
--   and an scalar-mult. (.*, *.) with the laws:
-- > a .* one == a
-- > (a + b) .* c == (a .* c) + (b .* c)
-- > c *. (a + b) == (c *. a) + (c *. b)
-- > a .* zero == zero
-- > a .* b == b *. a
class (Ring a, AbelianGroup (Sum (r a))) => Module r a
