{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -Wall #-}

-- | Algebra for Modules
module NumHask.Algebra.Abstract.Module
  ( Module
  ) where

import NumHask.Algebra.Abstract.Multiplicative
import NumHask.Algebra.Abstract.Ring
import NumHask.Algebra.Abstract.Action

-- | A <https://en.wikipedia.org/wiki/Module_(mathematics) Module> over r a is
--   a (Ring a), an abelian (Group r a) and an scalar-mult. (.*, *.) with the
--   laws:
--
-- > a .* one == a
-- > (a + b) .* c == (a .* c) + (b .* c)
-- > c *. (a + b) == (c *. a) + (c *. b)
-- > a .* zero == zero
-- > a .* b == b *. a
class (Distributive (Actor h), Divisive h, MultiplicativeAction h) => Module h
