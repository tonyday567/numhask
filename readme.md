mhs

<https://github.com/augustss/MicroHs>


# building to mhs

## bug 1

CPP logic that is now only partial.

    mcabal: uncaught exception: error: "../MicroCabal/src/MicroCabal/Normalize.hs",34:24: fields "import" cannot be combined, values=(VItem "ghc2021-additions",VItem "ghc2024-additions")


## bug 2

Encounter with TypeFamilies

    mhs: uncaught exception: error: "src/NumHask/Algebra/Action.hs": line 32, col 3:
      found:    type
      expected: LIdent ( UQIdent [ literal ~ ! (# infixl infixr infix default ; eof
    
    mcabal: uncaught exception: error: "./lib/System/Process.hs",12:5: callCommand: failed 256, "mhs -Pnumhask-0.13.1.0 -odist-mcabal/numhask-0.13.1.0.pkg -i -isrc -idist-mcabal/autogen '-DVERSION_ghc_compat=\"0.5.0.0\"' '-DMIN_VERSION_ghc_compat(x,y,z)=((x)<0||(x)==0&&(y)<5||(x)==0&&(y)==5&&(z)<=0)' '-DVERSION_base=\"4.19.1.0\"' '-DMIN_VERSION_base(x,y,z)=((x)<4||(x)==4&&(y)<19||(x)==4&&(y)==19&&(z)<=1)' -a. NumHask NumHask.Algebra.Action NumHask.Algebra.Additive NumHask.Algebra.Field NumHask.Algebra.Group NumHask.Algebra.Lattice NumHask.Algebra.Metric NumHask.Algebra.Multiplicative NumHask.Algebra.Ring NumHask.Data.Complex NumHask.Data.Integral NumHask.Data.Positive NumHask.Data.Rational NumHask.Data.Wrapped NumHask.Exception NumHask.Prelude"

## mix-n-match approach

    #if defined(__GLASGOW_HASKELL__)
    #endif
    
    #if defined(__MHS__)
    #endif


## bug 3

mcabal &#x2013;ghc build

mcabal: uncaught exception: error: &ldquo;../MicroCabal/src/MicroCabal/Backend/GHC.hs&rdquo;,26:5: The Stackage snapshot files are for ghc-9.12.2, but the current compiler is ghc-9.10.3

Moved to a compile cycle of `mcabal --ghc build && mcabal build`

CI picked up styling and extension issues ...

## MHS2024 extensions

Tracked down typos in MicroHS extension support list.

    cabal check


