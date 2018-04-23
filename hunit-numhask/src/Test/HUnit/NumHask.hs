{-# language DeriveDataTypeable #-}
module Test.HUnit.NumHask (
    shouldBeAbout
  , FPE(..)
  ) where

import Control.Monad (unless)
import Control.Exception (Exception(..))
import Data.Typeable
import Control.Monad.Catch (MonadThrow(..), throwM)

import Test.HUnit

import Prelude hiding (Num(..), (/))
import NumHask.Algebra


-- | A notion of approximate equality that takes into account floating point precision
--
-- > shouldBeAbout (1/3 :: Float) (0.333333)
-- >
--
-- > shouldBeAbout (1/3 :: Float) (0.33333)
-- > *** Exception: NotAboutEqual "expected: 0.33333\n but got: 0.33333334"
shouldBeAbout :: (Show a, Epsilon a) => a -> a -> Assertion
shouldBeAbout actual expected = 
  unless (actual `aboutEqual` expected) $
    throwM $ NotAboutEqual ("expected: " ++ show expected ++ "\n but got: " ++ show actual)




-- | Floating point exceptions
data FPE = NotAboutEqual String -- ^ Failure of the @Epsilon@ approximate equality test ('aboutEqual')
  deriving (Eq, Show, Typeable)
instance Exception FPE
