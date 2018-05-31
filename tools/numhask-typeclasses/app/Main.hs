{-# language OverloadedStrings #-}
module Main where


import Options.Applicative
import Data.Semigroup

import Language.Haskell.Exts



-- main :: IO (Module SrcSpanInfo)
main = do
  (CLOptions fpath) <- execParser clOpts
  mod <- fromParseResult <$> parseFile fpath
  print $ head $ unpack mod

unpack modd = concat $ unpackTyClDecl <$> unpackModuleDecls modd
--   pure $ unpackTyClDecl `map` decls

unpackModuleDecls :: Module l -> [Decl l]
unpackModuleDecls moddecl = case moddecl of
  Module _ _ _ _ decls -> decls
  _ -> []

-- | Horrible hack to extract typeclass or instance declarations
unpackTyClDecl :: Decl l -> [Either (Decl l) (Decl l)]
unpackTyClDecl decl = case decl of
  cd@ClassDecl{} -> [Left cd]
  ci@InstDecl{} -> [Right ci]
  _ -> []


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
