{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Main where

import NumHask.Prelude
import Test.DocTest

main :: IO ()
main =
  doctest
  [ "src/NumHask/Prelude.hs",
    "src/NumHask/Algebra/Abstract/Additive.hs",
    "src/NumHask/Algebra/Abstract/Multiplicative.hs",
    "src/NumHask/Algebra/Abstract/Ring.hs",
    "src/NumHask/Algebra/Abstract/Field.hs",
    "src/NumHask/Algebra/Abstract/Module.hs"
  ]
