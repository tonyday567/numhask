numhask
===

[![Build Status](https://travis-ci.org/tonyday567/numhask.svg)](https://travis-ci.org/tonyday567/numhask) [![Gitter chat](https://badges.gitter.im/numhask/Lobby.png)](https://gitter.im/numhask/Lobby)

This package provides numeric classes alternate to the prelude specified in haskell98.

The numeric class constellation looks somewhat like:

![nh](other/nh.svg)

Usage
===

``` haskell
{-# LANGUAGE NegativeLiterals #-}
{-# LANGUAGE RebindableSyntax #-}
import NumHask.Prelude
```
See the documentation in the Numhask module for a detailed overview.

Develop
===

```
stack build --test --haddock --file-watch
```
