# numhask-typeclasses

Extract NumHask typeclass and instance structure using `haskell-src-exts`.

NB : very preliminary, PRs welcome

# Instructions

  > stack install

  > numhask-typeclasses -p <file path of module to parse>


# Example

  In the current version, only the first typeclass appearing in the module is processed.

  > numhask-typeclasses -p numhask/numhask/src/NumHask/Algebra/Additive.hs

  Just (ClassDecl () Nothing (DHApp () (DHead () (Ident () "AdditiveMagma")) (UnkindedVar () (Ident () "a"))) [] (Just [ClsDecl () (TypeSig () [Ident () "plus"] (TyFun () (TyVar () (Ident () "a")) (TyFun () (TyVar () (Ident () "a")) (TyVar () (Ident () "a")))))]))

  which is the AST of

    class AdditiveMagma a where
      plus :: a -> a -> a

  > numhask-typeclasses -p /Users/ocramz/Documents/Personal/Haskell/numhask/numhask/src/NumHask/Algebra/Ring.hs

  Just (ClassDecl () (Just (CxTuple () [ClassA () (UnQual () (Ident () "MultiplicativeAssociative")) [TyVar () (Ident () "a")],ClassA () (UnQual () (Ident () "MultiplicativeUnital")) [TyVar () (Ident () "a")],ClassA () (UnQual () (Ident () "Distribution")) [TyVar () (Ident () "a")]])) (DHApp () (DHead () (Ident () "Semiring")) (UnkindedVar () (Ident () "a"))) [] Nothing)

  which corresponds to

    class (MultiplicativeAssociative a, MultiplicativeUnital a, Distribution a) => Semiring a
      