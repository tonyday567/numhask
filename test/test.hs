{-# OPTIONS_GHC -Wall #-}

module Main where

import Prelude
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
    -- FIXME: When doctest hits this module, it can't resolve even the simplest instances ...
    -- "src/NumHask/Data/Rational.hs"
  ]
