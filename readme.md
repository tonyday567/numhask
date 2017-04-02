[numhask](https://tonyday567.github.com/numhask) [![Build Status](https://travis-ci.org/tonyday567/numhask.png)](https://travis-ci.org/tonyday567/numhask)
==================================================================================================================================================

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

