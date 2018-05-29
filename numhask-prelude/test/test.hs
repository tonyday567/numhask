{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wall #-}

module Main where

import NumHask.Prelude
import Test.DocTest

main :: IO ()
main = doctest ["src/NumHask/Examples.hs"]

