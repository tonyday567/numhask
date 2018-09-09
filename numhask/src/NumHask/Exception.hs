{-# OPTIONS_GHC -Wall #-}

module NumHask.Exception
  ( NumHaskException(..)
  , throw
  ) where

import qualified Prelude as P
import Control.Exception
import Data.Typeable (Typeable)

newtype NumHaskException = NumHaskException { errorMessage :: P.String }
  deriving (P.Show, Typeable)

instance Exception NumHaskException
