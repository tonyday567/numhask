{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

-- | multi-dimensional numbers with a shape

module NumHask.HasShape where

import Protolude (Int)

-- | Could possibly be integrated with 'Representable' instance creation
class HasShape f where
    type Shape f
    shape :: (HasShape f) => f -> Shape f
    ndim :: (HasShape f) => f -> Int

