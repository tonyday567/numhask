# Restoring doctest-parallel Test Suite

This document explains how to restore the `doctest-parallel` test suite to numhask.

## Background

The doctest-parallel test suite was removed in favor of `cabal-docspec` for CI verification. The doctest-parallel test executed all doctests in the library using the parallel doctest runner.

## How to Restore

### 1. Update numhask.cabal

Add the following test suite stanza back to `numhask.cabal`:

```cabal
test-suite doctests
  import: ghc-options-stanza
  default-language: GHC2024
  main-is: doctests.hs
  hs-source-dirs: test
  build-depends:
    QuickCheck >=2.14 && <2.17,
    base >=4.14 && <5,
    doctest-parallel >=0.3 && <0.5,

  default-extensions: RebindableSyntax
  ghc-options: -threaded
  type: exitcode-stdio-1.0
```

### 2. Restore Test File

Create or restore `test/doctests.hs` with the following content:

```haskell
module Main where

import System.Environment (getArgs)
import Test.DocTest (mainFromCabal)
import Prelude (IO, (=<<))

main :: IO ()
main = mainFromCabal "numhask" =<< getArgs
```

### 3. Run Tests Locally

```bash
cabal test doctests
```

## Why doctest-parallel Was Removed

The test suite was removed to streamline CI verification:
- `cabal-docspec` is simpler and more efficient for CI
- The doctest-parallel test could still be run locally if needed
- Keeping it in the repo added unnecessary dependency complexity for CI

## When You Might Need This

- Local development where parallel doctest execution is preferred
- Testing on specific platforms or configurations
- Validating doctest behavior under specific conditions
