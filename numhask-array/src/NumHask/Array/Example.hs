{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- | Experimental api following https://pechersky.github.io/haskell-numpy-docs/quickstart.basics.html
module NumHask.Array.Example
  (
    -- * The Basics
    -- $setup

    -- ** An Example
    -- $anExample

    -- ** Array Creation
    -- $arrayCreation

    -- ** Printing Arrays
    -- $printingArrays

    -- ** Universal Functions
    -- $universalFunctions

    -- ** Indexing, Slicing and Iterating
    -- $indexingSlicingIterating

    -- * Shape Manipulation
    -- $shapeManipulation

    -- * Fancy indexing and index tricks
    -- $fancyIndexing

    -- * Linear Algebra
    -- $linearAlgebra

    -- * Tricks and Tips
    -- $tricksTips
  )
where

import NumHask.Shape
import NumHask.Array.Simple
import NumHask.Prelude as P

-- $setup
-- >>> :set -XDataKinds
-- >>> :set -XOverloadedLists
-- >>> :set -XNoImplicitPrelude
-- >>> :set -XFlexibleContexts
-- >>> import NumHask.Array.Simple as A
-- >>> import GHC.Exts (fromList)

-- $anExample
-- construction can be lazy; and zero pads
--
-- >>> let z = [] :: Array '[2] Int
-- >>> z
-- [0, 0]
-- >>> let a = [0..] :: Array '[3,5] Int
-- >>> a
-- [[0, 1, 2, 3, 4],
--  [5, 6, 7, 8, 9],
--  [10, 11, 12, 13, 14]]
-- >>> shape a
-- [3,5]
-- >>> length (shape a) -- dimension
-- 2
-- >>> :t a
-- a :: Array '[3, 5] Int

-- $arrayCreation
--
-- >>> let a = [2, 3, 4] :: Array '[3] Int
-- >>> a
-- [2, 3, 4]
-- >>> [1.2, 3.5, 5.1] :: Array '[3] Double
-- [1.2, 3.5, 5.1]
--
-- >>> -- lists of lists is not a thing, and need to be flattened
-- >>> let ls = [[1.0,2.0,3.0],[4.0,5.0,6.0]] :: [[Double]]
-- >>> fromList (concat ls) :: Array '[2,3] Double
-- [[1.0, 2.0, 3.0],
--  [4.0, 5.0, 6.0]]
--
-- >>> fromList ((\x -> (fromIntegral x) :+ zero) <$> [1,2,3,4]) :: Array '[2,2] (Complex Double)
-- [[1.0 :+ 0.0, 2.0 :+ 0.0],
--  [3.0 :+ 0.0, 4.0 :+ 0.0]]
-- >>> let z = [] :: Array '[3,4] Int
-- >>> z
-- [[0, 0, 0, 0],
--  [0, 0, 0, 0],
--  [0, 0, 0, 0]]
-- >>> let o = A.singleton one :: Array '[2,3,4] Int
-- >>> o
-- [[[1, 1, 1, 1],
--   [1, 1, 1, 1],
--   [1, 1, 1, 1]],
--  [[1, 1, 1, 1],
--   [1, 1, 1, 1],
--   [1, 1, 1, 1]]]
-- >>> let empt = A.singleton nan :: Array '[2,3] Double
-- >>> empt
-- [[NaN, NaN, NaN],
--  [NaN, NaN, NaN]]
--
-- >>>  [10,15 .. 30] :: Array '[4] Int
-- [10, 15, 20, 25]
-- >>> [0, 0.3.. 2] :: Array '[7] Double
-- [0.0, 0.3, 0.6, 0.8999999999999999, 1.2, 1.5, 1.7999999999999998]
--
-- > todo: fix NumHask.Range grid
-- > fromList (grid OuterPos (Range 0 2) 8) :: Array '[9] Double
-- > [0.0, 0.25, 0.5, 0.75, 1.0, 1.25, 1.5, 1.75, 2.0]
-- > let x = fromList (grid OuterPos (Range 0 (2*pi)) 100) :: Array '[101] Double
-- > let f = fmap sin x
--

-- $printingArrays
-- >>> show ([0..] :: Array '[6] Int) :: Text
-- "[0, 1, 2, 3, 4, 5]"
-- >>> [0..] :: Array '[2,3] Int
-- [[0, 1, 2],
--  [3, 4, 5]]
-- >>> [0..] :: Array '[1,2,3,1] Int
-- [[[[0],
--    [1],
--    [2]],
--   [[3],
--    [4],
--    [5]]]]
--
-- > todo: implement display
-- > import Formatting
-- > display (left 7 . fixed 2) ", " (fromList $ fromIntegral <$> [0..] :: Array [] '[100,100] Double)
-- > [[   0.00,    1.00,    2.00 ..   98.00   99.00],
-- >  [ 100.00,  101.00,  102.00 ..  198.00  199.00],
-- >  [ 200.00,  201.00,  202.00 ..  298.00  299.00],
-- >  ..
-- >  [9800.00, 9801.00, 9802.00 .. 9898.00 9899.00],
-- >  [9900.00, 9901.00, 9902.00 .. 9998.00 9999.00]]
--

-- $basicOperation
--
-- >>> let a = [20,30,40,50] :: Array '[4] Double
-- >>> let b = [0..] :: Array '[4] Double
-- >>> let c = a - b
-- >>> c
-- [20.0, 29.0, 38.0, 47.0]
-- >>> -- todo: resolve potential to polymorph number literals eg b**2
-- >>> b ** (one+one)
-- [0.0, 1.0, 4.0, 9.000000000000002]
-- >>> 10 *. (sin <$> a)
-- [9.129452507276277, -9.880316240928618, 7.451131604793488, -2.6237485370392877]
-- >>> (<35) <$> a
-- [True, True, False, False]
--
-- >>> let a = [0..] :: Array '[2,3] Int
-- >>> let b = [0..] :: Array '[3,2] Int
-- >>> a .*. a
-- [[0, 1, 4],
--  [9, 16, 25]]
-- >>> -- todo: resolve NumHask.Array and NumHask.Matrix operation names
-- >>> a `NumHask.Array.mmult` b
-- [[10, 13],
--  [28, 40]]
-- >>> a .* 2
-- [[0, 2, 4],
--  [6, 8, 10]]
--
-- >>> -- random example skipped
--
-- > -- todo: awaiting grid fix
-- > let a = singleton one :: Array '[3] Double
-- > let b = fromList (grid OuterPos (Range 0 pi) 2) :: Array '[3] Double
-- > let c = a + b
-- > let d = exp . ((zero:+one) *.) $ (:+zero) <$> c
-- > d
-- [0.5403023058681398 :+ 0.8414709848078965, (-0.8414709848078965) :+ 0.5403023058681398, (-0.5403023058681399) :+ (-0.8414709848078964)]
-- > :t d
-- d :: Array '[3] (Complex Double)
--
-- >>> -- folding
-- >>> let a = [0..] :: Array '[2,5] Int
-- >>> sum a
-- 45
-- >>> minimum a
-- 0
-- >>> maximum a
-- 9
-- >>> -- todo: scanAlong
--

-- $universalFunctions
--
-- >>> let a = [0..] :: Array '[3] Double
-- >>> exp <$> a
-- [1.0, 2.718281828459045, 7.38905609893065]
-- >>> sqrt <$> a
-- [0.0, 1.0, 1.4142135623730951]
--

-- $indexingSlicingIterating
--
-- >>> let a = (\x -> x*x*x) <$> [0..] :: Array '[10] Int
-- >>> index a [2]
-- 8
-- >>> let s = (\i -> index a [i]) <$> [2..5] :: Array '[4] Int
-- >>> s
-- [8, 27, 64, 125]
-- >>> :t s
-- s :: Array [] '[4] Int
-- >>> -- replace every second number with -1000
-- >>> let a' = (tabulate (\[i] -> if i `mod` 2 == 0 then -1000 else (index a [i]))) :: Array '[10] Int
-- >>> a'
-- [-1000, 1, -1000, 27, -1000, 125, -1000, 343, -1000, 729]
--
-- > -- todo: reverse fix
-- > let a'' = (let (n:_) = shape a in tabulate (\[i] -> index a [n-i])) :: Array '[4] Int
-- > a''
-- > [729, -1000, 343, -1000, 125, -1000, 27, -1000, 1, -1000]
--
-- > -- todo: slicing api

-- $shapeManipulation
--
-- > -- todo:

-- $fancyIndexing
--
-- > -- todo:

-- $linearAlgebra
--
-- > -- todo:

-- $tricksTips
--
-- > -- todo:

