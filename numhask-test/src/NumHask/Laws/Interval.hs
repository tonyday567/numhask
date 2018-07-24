{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE IncoherentInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module NumHask.Laws.Interval
  ( additiveIntervalLaws
  , multiplicativeIntervalLaws
  , divisiveIntervalLaws
  , distributiveIntervalLaws
  , fieldIntervalLaws
  , measureIntervalLaws
  , extraIntervalLaws
  , complexIntervalLaws
  )
where

import NumHask.Prelude
import NumHask.Laws
import NumHask.Data.Interval

-- | additive laws within an interval bound
additiveIntervalLaws
  ::
    ( Interval' a
    , Epsilon a
    , Multiplicative a
    , Subtractive a
    )
  => a -> [Law a]
additiveIntervalLaws accuracy =
  [ ( "associative: (a + b) + c = a + (b + c)"
    , Ternary (\a b c ->   ((a + b) + c) `member` (eps accuracy a + (eps accuracy b + eps accuracy c))
                        && (a + (b + c)) `member` ((eps accuracy a + eps accuracy b) + eps accuracy c))
    )
  , ( "left id: zero + a = a"
    , Unary (\a -> (zero + a) `member` eps accuracy a))
  , ( "right id: a + zero = a"
    , Unary (\a -> (a + zero) `member` eps accuracy a))
  , ( "commutative: a + b == b + a"
    , Binary (\a b -> (a + b) `member` (eps accuracy b + eps accuracy a)))
  ]

-- | multiplicative laws within an interval bound
multiplicativeIntervalLaws
  :: ( Interval' a
    , Multiplicative a
    , Subtractive a
    , Ord a
    , Epsilon a)
  => a -> [Law a]
multiplicativeIntervalLaws accuracy =
  [ ( "associative: (a * b) * c = a * (b * c)"
    , Ternary (\a b c ->   ((a * b) * c) `member` (eps accuracy a * (eps accuracy b * eps accuracy c))
                        && (a * (b * c)) `member` ((eps accuracy a * eps accuracy b) * eps accuracy c))
    )
  , ( "left id: zero * a = a"
    , Unary (\a -> (one * a) `member` eps accuracy a))
  , ( "right id: a * zero = a"
    , Unary (\a -> (a * one) `member` eps accuracy a))
  , ( "commutative: a * b == b * a"
    , Binary (\a b -> (a * b) `member` (eps accuracy b * eps accuracy a)))
  ]

{-
-- | multiplicative laws within an interval bound
multiplicativeComplexIntervalLaws
  :: (Interval' (Complex a), Interval' a, Ring a, Ord a, Signed a, Epsilon a
    , Multiplicative (Complex a)
    , Subtractive (Complex a))
  => Complex a -> [Law (Complex a)] 
multiplicativeComplexIntervalLaws accuracy =
  [ ( "associative: (a * b) * c = a * (b * c)"
    , Ternary (\a b c -> ((a * b) * c) `member`
                (eps accuracy a * (eps accuracy b * eps accuracy c))
                && (a * (b * c)) `member`
                ((eps accuracy a * eps accuracy b) * eps accuracy c))
    )
  , ( "left id: zero * a = a"
    , Unary (\a -> (one * a) `member` eps accuracy a))
  , ( "right id: a * zero = a"
    , Unary (\a -> (a * one) `member` eps accuracy a))
  , ( "commutative: a * b == b * a"
    , Binary (\a b -> (a * b) `member` (eps accuracy b * eps accuracy a)))
  ]


-}

divisiveIntervalLaws
  :: ( Interval' a, Ord a, Epsilon a
    , Divisive a
    , BoundedField a)
  => a -> [Law a]
divisiveIntervalLaws accuracy =
  [ ( "divide: a == zero || a / a == one"
    , Unary (\a ->  zero `member` (eps accuracy a)
                  || one `member` (eps accuracy a / eps accuracy a))
    )
  , ( "recip divide: recip a == one / a"
    , Unary (\a ->   a `member` (eps accuracy zero)
                  || (recip a) `member` ((eps accuracy one) / (eps accuracy a)))
    )
  , ( "recip left: a == zero || recip a * a == one"
    , Unary (\a ->   a `member` (eps accuracy zero)
                  || (recip a * a) `member` (eps accuracy one))
    )
  , ( "recip right: a == zero || a * recip a == one"
    , Unary (\a ->   a `member` (eps accuracy zero)
                  || (a * recip a) `member` (eps accuracy one))
    )
  ]

{-
divisiveComplexIntervalLaws
  :: ( Interval' a, Ord a, Signed a, Epsilon a
    , Divisive a
    , BoundedField a)
  => Complex a -> a -> [Law (Complex a)]
divisiveComplexIntervalLaws accComplex accReal=
  [ ( "divide: a == zero || a / a == one"
    , Unary (\a ->  zero `member` (eps (accReal:+zero) a)
                  || one `member` (eps (accReal:+zero) a / eps (accReal:+zero) a))
    )
  , ( "recip divide: recip a == one / a"
    , Unary (\a ->   a `member` (eps accComplex zero)
                  || (recip a) `member` ((eps accComplex one) / (eps accComplex a)))
    )
  , ( "recip left: a == zero || recip a * a == one"
    , Unary (\a ->   a `member` (eps (accReal:+zero) zero)
                  || (recip a * a) `member` (eps (accReal:+zero) one))
    )
  , ( "recip right: a == zero || a * recip a == one"
    , Unary (\a ->   a `member` (eps (accReal:+zero) zero)
                  || (a * recip a) `member` (eps (accReal:+zero) one))
    )
  ]

-}

distributiveIntervalLaws
  :: ( Interval' a, Ord a, Epsilon a, Invertible (Sum a)
    , Distributive a) => a -> [Law a]
distributiveIntervalLaws accuracy =
  [ ("left annihilation: a * zero == zero", Unary (\a -> (a * zero) `member` eps accuracy zero))
  , ("right annihilation: zero * a == zero", Unary (\a -> (zero * a) `member` eps accuracy zero))
  , ( "left distributivity: a * (b + c) == a * b + a * c"
    , Ternary (\a b c -> (a * (b + c)) `member` (eps accuracy a * eps accuracy b + eps accuracy a * eps accuracy c))
    )
  , ( "right distributivity: (a + b) * c == a * c + b * c"
    , Ternary (\a b c -> ((a + b) * c) `member` (eps accuracy a * eps accuracy c + eps accuracy b * eps accuracy c))
    )
  ]

{-
distributiveIntervalComplexLaws
  :: ( Interval' a, Ord a, Epsilon a, Signed a, Subtractive a
    , Distributive a) => Complex a -> [Law (Complex a)]
distributiveIntervalComplexLaws accuracy =
  [ ("left annihilation: a * zero == zero", Unary (\a -> (a * zero) `member` eps accuracy zero))
  , ("right annihilation: zero * a == zero", Unary (\a -> (zero * a) `member` eps accuracy zero))
  , ( "left distributivity: a * (b + c) == a * b + a * c"
    , Ternary (\a b c -> (a * (b + c)) `member` (eps accuracy a * eps accuracy b + eps accuracy a * eps accuracy c))
    )
  , ( "right distributivity: (a + b) * c == a * c + b * c"
    , Ternary (\a b c -> ((a + b) * c) `member` (eps accuracy a * eps accuracy c + eps accuracy b * eps accuracy c))
    )
  ]

-}

fieldIntervalLaws
  :: ( Epsilon a
    , Interval' a
    , Ord a
    , Field a
    , BoundedField a)
  => [Laws a a]
fieldIntervalLaws =
  (Arity1 <$> additiveIntervalLaws one) <>
  (Arity1 <$> subtractiveLaws) <>
  (Arity1 <$> multiplicativeIntervalLaws one) <>
  (Arity1 <$> divisiveIntervalLaws one) <>
  (Arity1 <$> distributiveIntervalLaws one)

extraIntervalLaws
  :: ( Epsilon a
    , Ord a
    , BoundedField a
    , Normed a a
    , FromRatio a
    , FromInteger a
    , ToRatio a
    , ExpField a)
  => [Laws a a]
extraIntervalLaws =
  (Arity1 <$> lowerBoundedFieldLaws) <>
  (Arity1 <$> upperBoundedFieldLaws) <>
  (Arity2 <$> expFieldLaws) <>
  (Arity1 <$> rationalLaws)

measureIntervalLaws
  :: ( Epsilon a
    , Ord a
    , Field a
    , Signed a
    , Normed a a
    , Metric a a
    , FromRatio a
    )
  => [Laws a a]
measureIntervalLaws =
  (Arity1 <$> signedLaws) <>
  (Arity2 <$> normedLaws) <>
  (Arity2 <$> metricRationalLaws)

complexIntervalLaws
  :: ( Epsilon a
    , Ord a
    , Interval' a
    , Field a
    , Signed a
    )
  => Complex a -> a -> [Laws (Complex a) (Complex a)]
complexIntervalLaws acc _ =
  (Arity1 <$> additiveIntervalLaws acc) <>
  (Arity1 <$> subtractiveLaws)
  -- (Arity1 <$> multiplicativeComplexIntervalLaws acc) <>
  -- (Arity1 <$> divisiveComplexIntervalLaws acc accReal) <>
  -- (Arity1 <$> distributiveIntervalComplexLaws acc)
