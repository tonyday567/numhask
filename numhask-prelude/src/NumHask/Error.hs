{-# OPTIONS_GHC -Wno-deprecations #-}

module NumHask.Error where

import Protolude
import Protolude.Error (error)

impossible :: Text -> a
impossible msg = error ("[impossible]" <> msg)
