{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# OPTIONS_GHC -Wno-orphans #-}

import NumHask.Array
import NumHask.Prelude
import Options.Generic
import Perf
import Perf.Analysis
import qualified Data.Matrix as Matrix
import qualified Numeric.LinearAlgebra as H
import qualified Data.Vector as V

data Opts = Opts
  { runs :: Maybe Int -- <?> "number of runs"
  , size :: Maybe Int -- <?> "size of matrix"
  } deriving (Generic, Show)

instance ParseRecord Opts

main :: IO ()
main = do
  o :: Opts <- getRecord "benchmarking numhask array"
  let !n = fromMaybe 1000 (runs o)
  let !sz = fromMaybe 10 (size o)

  _ <- warmup 100

  let aa = [1..] :: Array [] '[10,10] Int
  let ab = [0..] :: Array [] '[10,10] Int

  let ha = (sz H.>< sz) [1::H.Z ..]
  let hb = (sz H.>< sz) [1::H.Z ..]

  let ma = Matrix.matrix sz sz (\(i,j) -> i+sz*j)
  let mb = Matrix.matrix sz sz (\(i,j) -> i+sz*j)

  let va = [1..] :: Array V.Vector '[10,10] Int
  let vb = [0..] :: Array V.Vector '[10,10] Int

  (tcreatea, aa') <- tickIO $ pure aa
  (tcreateha, ha') <- tickIO $ pure ha
  (tcreatema, ma') <- tickIO $ pure ma
  (tcreateva, va') <- tickIO $ pure va
  (rmmult, _) <- ticks n (NumHask.Array.mmult aa') ab
  (rmmulth, _) <- ticks n (ha' H.<>) hb
  (rmmultm, _) <- ticks n (ma' `Matrix.multStd2`) mb
  (rmmultv, _) <- ticks n (NumHask.Array.mmult va') vb

  writeFile "other/array.md" $
    code
      [ "square matrix size: " <> show sz
      , ""
      , "creation"
      , formatInt "hmatrix:" 2 tcreateha
      , formatInt "matrix:" 2 tcreatema
      , formatInt "Array []:" 2 tcreatea
      , formatInt "Array Vector(Boxed):" 2 tcreateva
      , ""
      , "mmult"
      , formatRunHeader
      , ""
      , formatRun "hmatrix" 2 rmmulth
      , formatRun "matrix" 2 rmmultm
      , formatRun "[]" 2 rmmult
      , formatRun "Boxed" 2 rmmultv
      ]

  (rrow, _) <- ticks n (NumHask.Array.row (Proxy :: Proxy 4)) ab
  (rcol, _) <- ticks n (NumHask.Array.col (Proxy :: Proxy 4)) ab
  (runsaferow, _) <- ticks n (NumHask.Array.unsafeRow 0) ab
  (runsafecol, _) <- ticks n (NumHask.Array.unsafeCol 0) ab
  (runsafeindex, _) <- ticks n (NumHask.Array.unsafeIndex ab) [2,3]
  (rconcat, _) <- ticks n (concatenate (Proxy :: Proxy 2) aa) aa
  (rtranspose, _) <- ticks n NumHask.Array.transpose aa

  writeFile "other/ops.md" $
    code
      [ "square matrix size: " <> show sz
      , formatRunHeader
      , ""
      , formatRun "row" 2 rrow
      , formatRun "col" 2 rcol
      , formatRun "unsafeRow" 2 runsaferow
      , formatRun "unsafeCol" 2 runsafecol
      , formatRun "unsafeIndex" 2 runsafeindex
      , formatRun "concat" 2 rconcat
      , formatRun "transpose" 2 rtranspose
      ]

  pure ()
