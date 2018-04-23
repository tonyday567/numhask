{-# OPTIONS_GHC -Wno-deprecations #-}
module NumHask.Error where

import Protolude
import Protolude.Panic (panic)

impossible :: HasCallStack => Text -> a
impossible = panic
