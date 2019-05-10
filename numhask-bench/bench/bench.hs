{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

import NumHask.Array
import NumHask.Prelude
import Options.Generic
import Perf
import Perf.Analysis
import qualified Data.Matrix as Matrix
import qualified Numeric.LinearAlgebra as H
import qualified Data.Vector as V
import qualified Statistics.Matrix as DLA

instance NFData DLA.Matrix where
  rnf m@(DLA.Matrix r c v) = seq m ()

data Opts = Opts
  { runs :: Maybe Int -- <?> "number of runs"
  -- , size :: Maybe Int -- <?> "size of matrix"
  } deriving (Generic, Show)

instance ParseRecord Opts

main :: IO ()
main = do
  o :: Opts <- getRecord "benchmarking numhask array"
  let !n = fromMaybe 100 (runs o)
  _ <- warmup 100

  -- sz = 10 run
  let sz = 10

  let aa = [1 ..] :: Array [] '[10, 10] Double
  let ab = [0 ..] :: Array [] '[10, 10] Double

  let ha = (sz H.>< sz) [1 :: H.Z ..]
  let hb = (sz H.>< sz) [1 :: H.Z ..]

  let ma = Matrix.matrix sz sz (\(i, j) -> i + sz * j)
  let mb = Matrix.matrix sz sz (\(i, j) -> i + sz * j)

  let va = [1 ..] :: Array V.Vector '[10, 10] Double
  let vb = [0 ..] :: Array V.Vector '[10, 10] Double

  let dlaa = DLA.fromList sz sz $ [1 .. (fromIntegral $ sz*sz)]
  let dlab = DLA.fromList sz sz $ [0 .. (fromIntegral $ sz*sz - 1)]

  (tcreatea, aa') <- tickIO $ pure aa
  (tcreateha, ha') <- tickIO $ pure ha
  (tcreatema, ma') <- tickIO $ pure ma
  (tcreateva, va') <- tickIO $ pure va
  (tcreatedlaa, dlaa') <- tickIO $ pure dlaa
  (rmmult, _) <- ticks n (NumHask.Array.mmult aa') ab
  (rmmulth, _) <- ticks n (ha' H.<>) hb
  (rmmultm, _) <- ticks n (ma' `Matrix.multStd2`) mb
  (rmmultv, _) <- ticks n (NumHask.Array.mmult va') vb
  (rmmultdla, _) <- ticks n (DLA.multiply dlaa') dlab

  writeFile "other/array.md" $ code
    [ "square matrix size: " <> show sz
    , ""
    , "creation"
    , formatInt "hmatrix:" 2 tcreateha
    , formatInt "matrix:" 2 tcreatema
    , formatInt "numhask []:" 2 tcreatea
    , formatInt "numhask Boxed:" 2 tcreateva
    , formatInt "DLA:" 2 tcreatedlaa
    , ""
    , "mmult"
    , formatRunHeader
    , ""
    , formatRun "hmatrix" 2 rmmulth
    , formatRun "matrix" 2 rmmultm
    , formatRun "numhask []" 2 rmmult
    , formatRun "numhask Boxed" 2 rmmultv
    , formatRun "DLA" 2 rmmultdla
    ]

  -- sz = 20 run
  let sz = 20

  let aa = [1 ..] :: Array [] '[20, 20] Double
  let ab = [0 ..] :: Array [] '[20, 20] Double

  let ha = (sz H.>< sz) [1 :: H.Z ..]
  let hb = (sz H.>< sz) [1 :: H.Z ..]

  let ma = Matrix.matrix sz sz (\(i, j) -> i + sz * j)
  let mb = Matrix.matrix sz sz (\(i, j) -> i + sz * j)

  let va = [1 ..] :: Array V.Vector '[20, 20] Double
  let vb = [0 ..] :: Array V.Vector '[20, 20] Double

  let dlaa = DLA.fromList sz sz $ [1 .. (fromIntegral $ sz*sz)]
  let dlab = DLA.fromList sz sz $ [0 .. (fromIntegral $ sz*sz - 1)]

  (tcreatea, aa') <- tickIO $ pure aa
  (tcreateha, ha') <- tickIO $ pure ha
  (tcreatema, ma') <- tickIO $ pure ma
  (tcreateva, va') <- tickIO $ pure va
  (tcreatedlaa, dlaa') <- tickIO $ pure dlaa
  (rmmult, _) <- ticks n (NumHask.Array.mmult aa') ab
  (rmmulth, _) <- ticks n (ha' H.<>) hb
  (rmmultm, _) <- ticks n (ma' `Matrix.multStd2`) mb
  (rmmultv, _) <- ticks n (NumHask.Array.mmult va') vb
  (rmmultdla, _) <- ticks n (DLA.multiply dlaa') dlab

  appendFile "other/array.md" $ code
    [ "square matrix size: " <> show sz
    , ""
    , "creation"
    , formatInt "hmatrix:" 2 tcreateha
    , formatInt "matrix:" 2 tcreatema
    , formatInt "numhask []:" 2 tcreatea
    , formatInt "numhask Boxed:" 2 tcreateva
    , formatInt "DLA:" 2 tcreatedlaa
    , ""
    , "mmult"
    , formatRunHeader
    , ""
    , formatRun "hmatrix" 2 rmmulth
    , formatRun "matrix" 2 rmmultm
    , formatRun "numhask []" 2 rmmult
    , formatRun "numhask Boxed" 2 rmmultv
    , formatRun "DLA" 2 rmmultdla
    ]

  -- sz = 100 run
  let sz = 100

  let aa = [1 ..] :: Array [] '[100, 100] Double
  let ab = [0 ..] :: Array [] '[100, 100] Double

  let ha = (sz H.>< sz) [1 :: H.Z ..]
  let hb = (sz H.>< sz) [1 :: H.Z ..]

  let ma = Matrix.matrix sz sz (\(i, j) -> i + sz * j)
  let mb = Matrix.matrix sz sz (\(i, j) -> i + sz * j)

  let va = [1 ..] :: Array V.Vector '[100, 100] Double
  let vb = [0 ..] :: Array V.Vector '[100, 100] Double

  let dlaa = DLA.fromList sz sz $ [1 .. (fromIntegral $ sz*sz)]
  let dlab = DLA.fromList sz sz $ [0 .. (fromIntegral $ sz*sz - 1)]

  (tcreatea, aa') <- tickIO $ pure aa
  (tcreateha, ha') <- tickIO $ pure ha
  (tcreatema, ma') <- tickIO $ pure ma
  (tcreateva, va') <- tickIO $ pure va
  (tcreatedlaa, dlaa') <- tickIO $ pure dlaa
  (rmmult, _) <- ticks n (NumHask.Array.mmult aa') ab
  (rmmulth, _) <- ticks n (ha' H.<>) hb
  (rmmultm, _) <- ticks n (ma' `Matrix.multStd2`) mb
  (rmmultv, _) <- ticks n (NumHask.Array.mmult va') vb
  (rmmultdla, _) <- ticks n (DLA.multiply dlaa') dlab

  appendFile "other/array.md" $ code
    [ "square matrix size: " <> show sz
    , ""
    , "creation"
    , formatInt "hmatrix:" 2 tcreateha
    , formatInt "matrix:" 2 tcreatema
    , formatInt "numhask []:" 2 tcreatea
    , formatInt "numhask Boxed:" 2 tcreateva
    , formatInt "DLA:" 2 tcreatedlaa
    , ""
    , "mmult"
    , formatRunHeader
    , ""
    , formatRun "hmatrix" 2 rmmulth
    , formatRun "matrix" 2 rmmultm
    , formatRun "numhask []" 2 rmmult
    , formatRun "numhask Boxed" 2 rmmultv
    , formatRun "DLA" 2 rmmultdla
    ]

  -- numhask operations
  (rrow, _) <- ticks n (NumHask.Array.row (Proxy :: Proxy 4)) ab
  (rcol, _) <- ticks n (NumHask.Array.col (Proxy :: Proxy 4)) ab
  (runsaferow, _) <- ticks n (NumHask.Array.unsafeRow 0) ab
  (runsafecol, _) <- ticks n (NumHask.Array.unsafeCol 0) ab
  (runsafeindex, _) <- ticks n (NumHask.Array.unsafeIndex ab) [2, 3]
  (rconcat, _) <- ticks n (concatenate (Proxy :: Proxy 2) aa) aa
  (rtranspose, _) <- ticks n NumHask.Array.transpose aa

  writeFile "other/ops.md" $ code
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
