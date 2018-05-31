{-# language OverloadedStrings #-}
module Main where


import Options.Applicative
import Data.Semigroup

import Language.Haskell.Exts



main :: IO ()
main = do
  (CLOptions fpath) <- execParser clOpts
  mods <- fromParseResult <$> parseFile fpath
  print $ head $ unpack mods

-- unpack :: Module l -> [Decl l]
unpack modd = concat $ unpackTyClDecl <$> unpackModuleDecls modd

unpackModuleDecls :: Module l -> [Decl l]
unpackModuleDecls moddecl = case moddecl of
  Module _ _ _ _ decls -> decls
  _ -> []

-- | Horrible hack to extract typeclass declarations
-- unpackTyClDecl :: Decl l -> [(Decl l)]
unpackTyClDecl decl = case decl of
  (ClassDecl _ a b c d) -> [(a,b,c,d)]
  _ -> []



-- | Strip source position annotations 

-- uClassDecl c = case c of
--   ClsDecl _ ()




uContext :: Context l -> Context ()
uContext c = case c of
  CxSingle _ a -> CxSingle () $ uAsst a

uAsst :: Asst l -> Asst ()
uAsst a = case a of
  ClassA _ qn tys -> ClassA () (uQName qn) (map uType tys)
  AppA _ n tys -> AppA () (uName n) (map uType tys)
  InfixA _ ty0 qn ty -> InfixA () (uType ty0) (uQName qn) (uType ty)
  IParam _ ipn ty -> IParam () (uIPName ipn) (uType ty)
  EqualP _ ty ty1 -> EqualP () (uType ty) (uType ty1)
  ParenA _ asst -> ParenA () (uAsst asst)
  -- WildCardA _ mn ->

uIPName :: IPName l -> IPName ()
uIPName i = case i of
  IPDup _ s -> IPDup () s
  IPLin _ s -> IPLin () s

uType :: Type l -> Type ()
uType t = case t of
  -- TyForall _
  TyFun _ ty ty1 -> TyFun () (uType ty) (uType ty1)
  TyTuple _ b tys -> TyTuple () b (map uType tys)

uQName :: QName l -> QName ()
uQName qn = case qn of
  Qual _ mn n -> Qual () (uModuleName mn) (uName n)
  UnQual _ n -> UnQual () (uName n)
  Special _ sc -> Special () (uSpecialCon sc)

uSpecialCon :: SpecialCon l -> SpecialCon ()
uSpecialCon sc = case sc of
  UnitCon _ -> UnitCon ()
  ListCon _ -> ListCon ()
  FunCon _ -> FunCon ()
  TupleCon _ b i -> TupleCon () b i
  Cons _ -> Cons ()
  UnboxedSingleCon _ -> UnboxedSingleCon ()
  ExprHole _ -> ExprHole ()
  
  
uModuleName :: ModuleName l -> ModuleName ()
uModuleName mn = case mn of
  ModuleName _ n -> ModuleName () n

uDeclHead :: DeclHead l -> DeclHead ()
uDeclHead dhs = case dhs of
  DHead _ n -> DHead () $ uName n
  DHInfix _ tyvb n -> DHInfix () (uTyVarBind tyvb) (uName n)
  DHParen _ dh -> DHParen () (uDeclHead dh)
  DHApp _ dh t -> DHApp () (uDeclHead dh) (uTyVarBind t)
  
uTyVarBind :: TyVarBind l -> TyVarBind ()
uTyVarBind t = case t of
  KindedVar _ n k -> KindedVar () (uName n) (uKind k)
  UnkindedVar _ n -> UnkindedVar () (uName n)

uName :: Name l -> Name ()
uName n = case n of
  Ident _ s -> Ident () s
  Symbol _ s -> Symbol () s



uKind :: Kind l -> Kind ()
uKind ks = case ks of
  KindStar _ -> KindStar ()
  KindFn _ k1 k2 -> KindFn () (uKind k1) (uKind k2)
  KindParen _ k -> KindParen () (uKind k)
  -- KindVar _          -- qualified names
  KindApp _ k1 k2 -> KindApp () (uKind k1) (uKind k2)
  KindTuple _ kz -> KindTuple () $ map uKind kz
  KindList _ k -> KindList () (uKind k)











-- * Command line option parsing


clOpts :: ParserInfo CLOptions
clOpts = info (clOptions <**> helper) (
  fullDesc <> progDesc "Analyze a NumHask module with haskell-src-exts"
    -- <> header ""
                                      )




data CLOptions = CLOptions {
  cloFilepath :: FilePath -- ^ file path of module to be analyzed
  } deriving (Eq, Show)


clOptions :: Parser CLOptions
clOptions = CLOptions <$>
  strOption (
     long "filepath" <>
     short 'p' <>
     metavar "FILEPATH" <>
     help "Filepath of the module to be analyzed" )
