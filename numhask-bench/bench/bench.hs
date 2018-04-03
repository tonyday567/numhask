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

import Options.Generic
import NumHask.Prelude hiding ((%))
import Perf hiding (zero)
import Formatting
import qualified Data.Text as Text
import NumHask.Array
import qualified Numeric.LinearAlgebra as H
import qualified Data.Matrix as Matrix
import Data.TDigest
import Data.Scientific
import qualified Protolude as P

instance ToInteger Cycle where
  toInteger = P.toInteger

-- | compute a percentile
--
-- > c <- percentile 0.4 . fst <$> ticks n f a
--
percentile :: (Functor f, Foldable f) => Double -> f Cycle -> Double
percentile p xs = fromMaybe 0 $ quantile p (tdigest (fromIntegral <$> xs) :: TDigest 25)

prec :: Int -> Format r (Scientific -> r)
prec n = scifmt Exponent (Just n)

int2Sci :: (ToInteger a) => a -> Scientific
int2Sci n = scientific (fromIntegral n) 0

double2Sci :: (RealFloat a) => a -> Scientific
double2Sci = fromFloatDigits

data Opts = Opts
  { runs :: Maybe Int -- <?> "number of runs"
  , sumTo :: Maybe Int -- <?> "sum to this number"
  } deriving (Generic, Show)

instance ParseRecord Opts

code :: [Text] -> Text
code cs = "\n```\n" <> Text.intercalate "\n" cs <> "\n```\n"

f :: Int -> Int
f x = foldl' (+) zero ([zero .. x] :: [Int])

formatRun :: [Cycle] -> Text -> Text
formatRun cs label =
    sformat
          ((right 24 ' ' %. stext) % stext %
           (left 7 ' ' %. prec 3) % " cycles")
          label
          (Text.intercalate " " $ sformat (left 7 ' ' %. prec 3) <$>
           int2Sci <$> take 5 cs)
          (double2Sci $ percentile 0.4 cs)

formatRunHeader :: Text
formatRunHeader =
        sformat
          ((right 24 ' ' %. stext) %
           (left 7 ' ' %. stext) %
           (left 8 ' ' %. stext) %
           (left 8 ' ' %. stext) %
           (left 8 ' ' %. stext) %
           (left 8 ' ' %. stext) %
           (left 8 ' ' %. stext))
          "run"
          "first"
          "2nd"
          "3rd"
          "4th"
          "5th"
          "40th %"

run :: Functor f => Text -> f ([Cycle], b) -> f Text
run label t = (`formatRun` label) . fst <$> t

main :: IO ()
main = do
  o :: Opts <- getRecord "an example app for readme-lhs"
  let n = fromMaybe 1000 (runs o)
  let a = fromMaybe 1000 (sumTo o)

  _ <- warmup 100

  let fInt x = foldr (+) zero ([one .. x] :: [Int])
  (t, _) <- tick f a
  (tInt, _) <- tick fInt a
  rf <- run "sum" $ ticks n f a
  writeFile "other/test.md" $
    code
      [ "sum to " <> show a
      , "first measure: " <> show t <> " cycles"
      , "fInt version: " <> show tInt <> " cycles"
      , rf
      ]

  let sz = 10
  let aa = [1..] :: Array [] '[10,10] Int
  let ab = [0..] :: Array [] '[10,10] Int

  let ha = (sz H.>< sz) [1::H.Z ..]
  let hb = (sz H.>< sz) [1::H.Z ..]
  
  let ma = Matrix.matrix sz sz (\(i,j) -> i+sz*j)
  let mb = Matrix.matrix sz sz (\(i,j) -> i+sz*j)

  (tcreatea, aa') <- tickIO $ pure aa
  (tcreateha, ha') <- tickIO $ pure ha
  (tcreatema, ma') <- tickIO $ pure ma
  rmmult <- run "mmult (sz,sz)" $ ticks n (NumHask.Array.mmult aa') ab
  rmmulth <- run "hmatrix mmult (sz,sz)" $ ticks n (ha' H.<>) hb
  rmmultm <- run "matrix mmult (sz,sz)" $ ticks n (ma' `Matrix.multStd2`) mb

  writeFile "other/array.md" $
    code
      [ "square matrix size: " <> show sz
      , ""
      , "creation array:   " <> sformat (prec 3) (int2Sci tcreatea)  <> " cycles"
      , "creation hmatrix: " <> sformat (prec 3) (int2Sci tcreateha) <> " cycles"
      , "creation matrix:  " <> sformat (prec 3) (int2Sci tcreatema) <> " cycles"
      , ""
      , formatRunHeader
      , ""
      , rmmult
      , rmmulth
      , rmmultm
      ]

  rrow <- run "row" $ ticks n (NumHask.Array.row (Proxy :: Proxy 4)) ab
  rcol <- run "col" $ ticks n (NumHask.Array.col (Proxy :: Proxy 4)) ab
  runsaferow <- run "unsafeRow" $ ticks n (NumHask.Array.unsafeRow 0) ab
  runsafecol <- run "unsafeCol" $ ticks n (NumHask.Array.unsafeCol 0) ab
  runsafeindex <- run "unsafeIndex" $ ticks n (NumHask.Array.unsafeIndex ab) [2,3]
  -- rindex <- run "index" $ ticks n (NumHask.Array.index ab) [2,3]
  -- rslice <- run "slice" $ ticks n
  --    (P.fromIntegral $ (NumHask.Array.slice (Proxy :: Proxy '[ '[0,1],'[2],'[1,2]]))) ab
  -- runsafeslice <- run "unsafeSlice" $ ticks n
  --    (NumHask.Array.unsafeSlice [[0,1],[2],[1,2]]) ab
  -- rmapalong <- run "mapAlong" $ ticks n
  --    (mapAlong (Proxy :: Proxy 0) (\x -> NumHask.Array.zipWith (*) x x)) ab
  rconcat <- run "concatenate" $ ticks n (concatenate (Proxy :: Proxy 2) aa) aa
  rtranspose <- run "transpose" $ ticks n NumHask.Array.transpose aa

  writeFile "other/ops.md" $
    code
      [ "square matrix size: " <> show sz
      , formatRunHeader
      , ""
      , rrow
      , rcol
      , runsaferow
      , runsafecol
      -- , rindex
      , runsafeindex
      -- , rslice
      -- , runsafeslice
      -- , rmapalong
      , rconcat
      , rtranspose
      ]

  pure ()
