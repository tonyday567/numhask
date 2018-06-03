{-# language OverloadedStrings #-}
module Main where

import Data.Maybe

import qualified Options.Applicative as O (Parser, ParserInfo, execParser, info, helper, fullDesc, progDesc, header, strOption, long, short, metavar, help)
import Options.Applicative ( (<**>) )
import Data.Semigroup

import qualified Data.Map as M


import GHC hiding (parseModule, ParsedSource(..))
import HsSyn (HsModule(..))
import HsDecls (HsDecl(..))

import Language.Haskell.GHC.ExactPrint

-- main = print "hello!"

main = do 
  (CLOptions fpath) <- O.execParser clOpts
  modse <- parseModule fpath
  case modse of
    Left (_, e) -> error e
    Right (anns, psource) -> pure psource



-- main :: IO ()
-- main = do
--   (CLOptions fpath) <- execParser clOpts
--   mods <- fromParseResult <$> parseFile fpath
--   let modh = stripAnnotations mods
--   print modh

-- stripAnnotations :: Module b -> Maybe (Decl ())
-- stripAnnotations mo = head $ filter isJust $ uTyClDecl `map` unpackModuleDecls mo

-- unpackModuleDecls :: Module l -> [Decl l]
-- unpackModuleDecls moddecl = case moddecl of
--   Module _ _ _ _ decls -> decls
--   _ -> []

-- -- | Strip source position annotations
-- uTyClDecl :: Decl b -> Maybe (Decl ())
-- uTyClDecl d = case d of
--   cd@ClassDecl{} -> Just $ const () <$> cd
--   _ -> Nothing
  




-- * Command line option parsing

clOpts :: O.ParserInfo CLOptions
clOpts = O.info (clOptions <**> O.helper) (
  O.fullDesc <> O.progDesc "Analyze a NumHask module with haskell-src-exts"
    -- <> O.header ""
                                      )

data CLOptions = CLOptions {
  cloFilepath :: FilePath -- ^ file path of module to be analyzed
  } deriving (Eq, Show)


clOptions :: O.Parser CLOptions
clOptions = CLOptions <$>
  O.strOption (
     O.long "filepath" <>
     O.short 'p' <>
     O.metavar "FILEPATH" <>
     O.help "Filepath of the module to be analyzed" )
