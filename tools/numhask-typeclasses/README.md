# numhask-typeclasses

Extract NumHask typeclass and instance structure using `haskell-src-exts`.

NB : very preliminary, PRs welcome

# Instructions

  > stack install

  > numhask-typeclasses -p <file path of module to parse>


# Example

  > numhask-typeclasses -p numhask/numhask/src/NumHask/Algebra/Additive.hs

  Just (ClassDecl () Nothing (DHApp () (DHead () (Ident () "AdditiveMagma")) (UnkindedVar () (Ident () "a"))) [] (Just [ClsDecl () (TypeSig () [Ident () "plus"] (TyFun () (TyVar () (Ident () "a")) (TyFun () (TyVar () (Ident () "a")) (TyVar () (Ident () "a")))))]))