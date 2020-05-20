{-# OPTIONS_GHC -Wno-deprecations #-}

module NumHask.Error where

import Protolude

impossible :: HasCallStack => Text -> a
impossible = panic
