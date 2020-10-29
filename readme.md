numhask
===

[![Build Status](https://travis-ci.org/tonyday567/numhask.svg)](https://travis-ci.org/tonyday567/numhask) [![Gitter chat](https://badges.gitter.im/numhask/Lobby.png)](https://gitter.im/numhask/Lobby)

A numeric class hierarchy, providing a structure for numbers and functions that combine them.

Usage
===

``` haskell
{-# LANGUAGE NegativeLiterals #-}
{-# LANGUAGE RebindableSyntax #-}
import NumHask.Prelude
```

Classes
===

[![Field Hierarchy](other/field.svg)](other/field.svg)

Prelude
===

numhask includes `NumHask.Prelude` which combines the numerical classes with [protolude](https://www.stackage.org/package/protolude).

Develop
===

```
stack build --test --haddock --file-watch
```
