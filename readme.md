mhs

<https://github.com/augustss/MicroHs>


# building to mhs

## cabal cpp bug

CPP logic in cabal file is now only partial.

    mcabal: uncaught exception: error: "../MicroCabal/src/MicroCabal/Normalize.hs",34:24: fields "import" cannot be combined, values=(VItem "ghc2021-additions",VItem "ghc2024-additions")


## TyepFamilies

Encounter with TypeFamilies

    mhs: uncaught exception: error: "src/NumHask/Algebra/Action.hs": line 32, col 3:
      found:    type
      expected: LIdent ( UQIdent [ literal ~ ! (# infixl infixr infix default ; eof
    
    mcabal: uncaught exception: error: "./lib/System/Process.hs",12:5: callCommand: failed 256, "mhs -Pnumhask-0.13.1.0 -odist-mcabal/numhask-0.13.1.0.pkg -i -isrc -idist-mcabal/autogen '-DVERSION_ghc_compat=\"0.5.0.0\"' '-DMIN_VERSION_ghc_compat(x,y,z)=((x)<0||(x)==0&&(y)<5||(x)==0&&(y)==5&&(z)<=0)' '-DVERSION_base=\"4.19.1.0\"' '-DMIN_VERSION_base(x,y,z)=((x)<4||(x)==4&&(y)<19||(x)==4&&(y)==19&&(z)<=1)' -a. NumHask NumHask.Algebra.Action NumHask.Algebra.Additive NumHask.Algebra.Field NumHask.Algebra.Group NumHask.Algebra.Lattice NumHask.Algebra.Metric NumHask.Algebra.Multiplicative NumHask.Algebra.Ring NumHask.Data.Complex NumHask.Data.Integral NumHask.Data.Positive NumHask.Data.Rational NumHask.Data.Wrapped NumHask.Exception NumHask.Prelude"

## implementing the CPP mix-n-match approach

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


## remove Naturals

    mhs: uncaught exception: error: "src/NumHask/Algebra/Additive.hs": line 15, col 8: Module not found: GHC.Natural
    search path=["src","dist-mcabal/autogen"]
    package path=["/Users/tonyday567/.mcabal/mhs-0.14.25.0","."]

## remove NumHask.Algebra.Group & KleeneAlgebra from library.

    mhs: uncaught exception: error: "src/NumHask/Algebra/Action.hs": line 56, col 50: kind error: cannot unify Type and (_a18 -> _a19)

## ormolu failed


    src/NumHask/Prelude.hs:25:1
       The GHC parser (in Haddock mode) failed:
       [GHC-58481] parse error (possibly incorrect indentation or mismatched brackets)  
  
## cannot unify type

    mhs: uncaught exception: error: "src/NumHask/Algebra/Action.hs": line 56, col 50: kind error: cannot unify Type and (_a18 -> _a19)
  

## included mhs CI

## more typefamilies and Natural usage sites

    mhs: uncaught exception: error: "src/NumHask/Algebra/Field.hs": line 120, col 3:
      found:    type
      expected: LIdent ( UQIdent [ literal ~ ! (# infixl infixr infix default ; eof
    
    mhs: uncaught exception: error: "src/NumHask/Data/Integral.hs": line 19, col 8: Module not found: GHC.Natural
    search path=["src","dist-mcabal/autogen"]
    package path=["/Users/tonyday567/.mcabal/mhs-0.14.25.0","."]


## QuotientField

I stopped here and switched to a new branch because I wanted to compile the __MHS__ wrapped code with ghc but couldnt work out an easy way to achieve this.

    mhs: uncaught exception: error: "src/NumHask/Algebra/Field.hs": line 202, col 18: Cannot satisfy constraint: (QuotientField _a3654 _a3655)
     fully qualified: (NumHask.Algebra.Field.QuotientField _a3654 _a3655)


## GHC.Enum

This branch has a lot of QuotientField ripped out.

mhs: uncaught exception: error: "src/NumHask/Algebra/Lattice.hs": line 21, col 8: Module not found: GHC.Enum
search path=["src","dist-mcabal/autogen"]
package path=["/Users/tonyday567/.mcabal/mhs-0.14.25.0","."]


## extension work

Created extension lists to check if both mhs and ghc extensions are compatible with all code.

## refactor of Algebra.Metric

    mhs: uncaught exception: error: "src/NumHask/Algebra/Metric.hs": line 67, col 3:
      found:    type
      expected: LIdent ( UQIdent [ literal ~ ! (# infixl infixr infix default ; eof
      
Swapping out TF for FD is major for Basis, Polar etc.

https://gitlab.haskell.org/ghc/ghc/-/wikis/tf-vs-fd

functional dependencies are type inference and type families are evidence for type equality.
FD works better with overlapping instances.

## derivingvia

Something weird is going on with derivingvia

    mhs: uncaught exception: error: "src/NumHask/Data/Complex.hs": line 51, col 17: Multiple constraint solutions for: (Additive (EuclideanPair a))

Even after adding an instance manually, up comes:

    mhs: uncaught exception: error: "src/NumHask/Data/Complex.hs": line 51, col 17: Cannot satisfy constraint: (Additive a)
         fully qualified: (NumHask.Algebra.Additive.Additive a)
         
Removing Data.Complex seemed like the only way to progress.

## Data.Rational

mhs didnt like identical constraints in a list.

    mhs: uncaught exception: error: "src/NumHask/Data/Rational.hs": line 96, col 100: Multiple constraint solutions for: (Basis a a a)
    mhs: uncaught exception: error: "src/NumHask/Data/Rational.hs": line 19, col 8: Module not found: GHC.Float
    
Ratio is in Data.Ratio_Type

    mhs: uncaught exception: error: "src/NumHask/Data/Rational.hs": line 205, col 37: undefined value: rationalToDouble


## direct name clashes

    mhs: uncaught exception: error: "src/NumHask.hs": line 88, col 5: ambiguous export: Integral [Data.Integral.Integral,NumHask.Data.Integral.Integral]
    mhs: uncaught exception: error: "src/NumHask.hs": line 101, col 5: ambiguous export: Rational [Data.Ratio_Type.Rational,NumHask.Data.Rational.Rational]
    mhs: uncaught exception: error: "src/NumHask.hs": line 22, col 5: ambiguous export: sum [Data.List.sum,NumHask.Algebra.Additive.sum]

nameclashes logged via tagging in NumHask.hs

## utf8 guff

mhs: uncaught exception: error: "src/NumHask/Data/Positive.hs": line 128, col 22:
  found:    Unrecognized input: '\8760'
  expected: LIdent ( UQIdent [ literal ~ ! (# QSymOper `

## weird Eq a constraint coming out of the woodwork.

mhs: uncaught exception: error: "src/NumHask/Data/Positive.hs": line 71, col 18: Cannot satisfy constraint: (Eq a)
     fully qualified: (Data.Eq.Eq a)

## overlodaed labels

    mhs: uncaught exception: error: "src/NumHask/Prelude.hs": line 68, col 8: Module not found: GHC.OverloadedLabels
    search path=["src","dist-mcabal/autogen"]
    package path=["/Users/tonyday567/.mcabal/mhs-0.14.25.0","."]


## unwound Natural CPP and put Natural back in with:

``` haskell
#if defined(__GLASGOW_HASKELL__)
import GHC.Natural (Natural (..))
#endif
#if defined(__MHS__)
import Numeric.Natural (Natural (..))
#endif
```

## audit

- [x] fromList fixed => get tests back???
  Removed from mhs (OverloadedLists)
- [x] GHC.Generics usage
  Spoofed by mhs
- [x] OverloadedLabels Usage
  Removed from ghc
- fix fromRational
- put ghc TypeFamily QuotientFields back
- put ghc TypeFamily Basis back
- check Direction
- fix ToRatio, FromRatio
- Positive bugs
- Polar bugs
- [x] Question GHC.Exts usage

## cabal run doctest errors with Not in scope: fromString, which is a rebindable syntax thing.

## removed RebindableSyntax

## turned a lot of doctests off.

removed GHC.Generics except at EuclideanPair site

## pushed meetLeq (Eq a) down to functions 

## The Epsilon (Eq a) bug

Adding Epsilon to the list of DerivingVias causes this error:

    mhs: uncaught exception: error: "src/NumHask/Data/Positive.hs": line 71, col 18: Cannot satisfy constraint: (Eq a)
     fully qualified: (Data.Eq.Eq a)
     
mhs fails to derivevia because of the (Eq a) constraint, where GHC finds it.

Seemed legit and adding in a manual instance.


## Putting back GHC TF-style classes and instances; QuotientField, FromRational, Basis & Direction

QuotientField for mhs a bit weird:

    mhs: uncaught exception: error: "src/NumHask/Data/Positive.hs": line 113, col 11: Cannot satisfy constraint: ((Positive Int) ~ w#10967)
     fully qualified: (Primitives.~ (NumHask.Data.Positive.Positive Primitives.Int) w#10967)


