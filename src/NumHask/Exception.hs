-- | Exceptions arising within numhask.
module NumHask.Exception
  ( NumHaskException (..),
    throw,
  )
where

import Control.Exception
import Prelude qualified as P

-- | A numhask exception.
newtype NumHaskException = NumHaskException {errorMessage :: P.String}
  deriving (P.Show)

instance Exception NumHaskException
