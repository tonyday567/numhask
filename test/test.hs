{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Main where

import NumHask.Prelude
import Test.DocTest

main :: IO ()
main =
  doctest
  [ "src/NumHask.hs",
    "src/NumHask/Prelude.hs",
    "src/NumHask/Algebra/Additive.hs",
    "src/NumHask/Algebra/Multiplicative.hs",
    "src/NumHask/Algebra/Ring.hs",
    "src/NumHask/Algebra/Field.hs",
    "src/NumHask/Algebra/Module.hs"
  ]
