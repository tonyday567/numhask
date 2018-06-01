{-# language OverloadedStrings #-}
module Main where

import Data.Maybe

import Options.Applicative
import Data.Semigroup

import Language.Haskell.Exts



main :: IO ()
main = do
  (CLOptions fpath) <- execParser clOpts
  mods <- fromParseResult <$> parseFile fpath
  -- print $ head $ unpack mods
  -- pure $ const () <$> unpack mods
  let modh = stripAnnotations mods
  -- pure $ map (\(a,b,c) -> const () <$> a <*> b <*> c) mods'
  -- pure mods'
  print modh

stripAnnotations :: Module b -> Maybe (Decl ())
stripAnnotations mo = head $ filter isJust $ uTyClDecl `map` unpackModuleDecls mo

unpackModuleDecls :: Module l -> [Decl l]
unpackModuleDecls moddecl = case moddecl of
  Module _ _ _ _ decls -> decls
  _ -> []

-- | Strip source position annotations
uTyClDecl :: Decl b -> Maybe (Decl ())
uTyClDecl d = case d of
  cd@ClassDecl{} -> Just $ const () <$> cd
  _ -> Nothing
  




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
