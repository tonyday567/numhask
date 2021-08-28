-- To run it:
-- ~/.ghcup/bin/cabal clean && ~/.ghcup/bin/cabal build  --with-ghc /opt/ghc/8.10.2/bin/ghc  all && ~/.cabal/bin/weeder-8.10

{ roots = [ "^NumHask.Algebra.Additive.sum$",
            "^NumHask.Algebra.Multiplicative.product$",
            "^NumHask.Algebra.Multiplicitive.odd$",
            "^NumHask.Algebra.Field.half$",
            "^NumHask.Algebra.Metric.coord$",
            "^NumHask.Algebra.Metric.distance$",
            "^NumHask.Algebra.Metric.polar$",
            "^NumHask.Algebra.Metric.~=$",
            "^NumHask.Algebra.Lattice.joinLeq$",
            "^NumHask.Data.Complex.imagPart$",
            "^NumHask.Data.Complex.realPart$",
            "^NumHask.Data.Integral.\\^$",
            "^NumHask.Data.Integral.\\^\\^$",
            "^NumHask.Data.Integral.odd$",
            "^NumHask.Prelude.ifThenElse$"
]
, type-class-roots = True }
