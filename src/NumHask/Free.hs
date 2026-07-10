{-# LANGUAGE NoRebindableSyntax #-}

-- | hasknum: initial encoding of numhask's numeric hierarchy.
--
-- Import the specific modules you need:
--
-- > import NumHask.Free.Additive
-- > import NumHask.Free.Multiplicative
-- > import NumHask.Free.Subtractive
-- > import NumHask.Free.Ring
-- > import NumHask.Free.StarSemiring
--
-- Each level exports the same vocabulary ('zero', 'plus', 'embed',
-- 'eval') over distinct types, so qualified imports are recommended
-- when mixing levels.
module NumHask.Free where
