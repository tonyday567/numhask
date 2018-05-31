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
