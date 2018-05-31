{-# language OverloadedStrings #-}
module Main where


import Options.Applicative
import Data.Semigroup

import Language.Haskell.Exts



-- main :: IO (Module SrcSpanInfo)
main = do
  (CLOptions fpath) <- execParser clOpts
  mod <- fromParseResult <$> parseFile fpath
  print mod



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
