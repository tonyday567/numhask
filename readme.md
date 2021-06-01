numhask
===

[![Hackage](https://img.shields.io/hackage/v/numhask.svg)](https://hackage.haskell.org/package/numhask)

This package provides numeric classes alternate to the prelude specified in haskell98.

The numeric class constellation looks somewhat like:

![nh](other/nh.svg)

Usage
===

``` haskell
{-# LANGUAGE RebindableSyntax #-}
import NumHask.Prelude
```
See the documentation in the Numhask module for a detailed overview.

NumHask.Prelude passes through the Prelude, with the exception of functions which include Num, Rational, Integral or Real constraints.

Develop
===

```
stack build --test --haddock --file-watch
```
