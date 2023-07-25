-- | Exceptions arising within numhask.
module NumHask.Exception
  ( NumHaskException (..),
    throw,
  )
where

import Control.Exception
import Data.Typeable (Typeable)
import qualified Prelude as P

-- | A numhask exception.
newtype NumHaskException = NumHaskException {errorMessage :: P.String}
  deriving (P.Show, Typeable)

instance Exception NumHaskException
