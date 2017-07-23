numhask
===

[![Build Status](https://travis-ci.org/tonyday567/numhask.svg)](https://travis-ci.org/tonyday567/numhask) [![Hackage](https://img.shields.io/hackage/v/numhask.svg)](https://hackage.haskell.org/package/numhask) [![lts](https://www.stackage.org/package/numhask/badge/lts)](http://stackage.org/lts/package/numhask) [![nightly](https://www.stackage.org/package/numhask/badge/nightly)](http://stackage.org/nightly/package/numhask) 

A numeric prelude for primitive numbers and representable objects.

``` {.sourceCode .literate .haskell}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DataKinds #-}
```

``` {.sourceCode .literate .haskell}
import Numhask.Prelude
```

`Numhask.Prelude` is a drop-in replacement for `Prelude`. Behind the
scenes, it wraps `Protolude`.

~~~
stack build --test
~~~

