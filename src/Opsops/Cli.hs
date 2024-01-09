{-# LANGUAGE OverloadedStrings #-}

-- | This module provides top-level definitions for the CLI program.
module Opsops.Cli where

import Control.Applicative ((<**>), (<|>))
import Control.Monad (join)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Opsops.Meta as Meta
import qualified Options.Applicative as OA
import System.Exit (ExitCode (..))


-- * Entrypoint


-- | CLI program entrypoint.
cli :: IO ExitCode
cli =
  join (OA.execParser (OA.info opts desc))
  where
    opts = optProgram <**> infoOptVersion <**> OA.helper
    desc =
      OA.fullDesc
        <> OA.progDesc "Top Level Commands"
        <> infoModHeader
        <> infoModFooter


-- * Program


-- | Option parser for top-level commands.
optProgram :: OA.Parser (IO ExitCode)
optProgram =
  commandGreet
    <|> commandFarewell


-- * Commands


-- ** greet


-- | Definition for @greet@ CLI command.
commandGreet :: OA.Parser (IO ExitCode)
commandGreet = OA.hsubparser (OA.command "greet" (OA.info parser infomod) <> OA.metavar "greet")
  where
    infomod = OA.fullDesc <> infoModHeader <> OA.progDesc "Greet user." <> OA.footer "This command prints a greeting message to the console."
    parser =
      doGreet
        <$> OA.strOption (OA.short 'n' <> OA.long "name" <> OA.value "World" <> OA.showDefault <> OA.help "Whom to greet.")


-- | @greet@ CLI command program.
doGreet :: T.Text -> IO ExitCode
doGreet n = do
  TIO.putStrLn ("Hello " <> n <> "!")
  pure ExitSuccess


-- ** farewell


-- | Definition for @farewell@ CLI command.
commandFarewell :: OA.Parser (IO ExitCode)
commandFarewell = OA.hsubparser (OA.command "farewell" (OA.info parser infomod) <> OA.metavar "farewell")
  where
    infomod = OA.fullDesc <> infoModHeader <> OA.progDesc "Say farewell to user." <> OA.footer "This command prints a farewell message to the console."
    parser =
      doFarewell
        <$> OA.strOption (OA.short 'n' <> OA.long "name" <> OA.value "World" <> OA.showDefault <> OA.help "Whom to say farewell to.")


-- | @farewell@ CLI command program.
doFarewell :: T.Text -> IO ExitCode
doFarewell n = do
  TIO.putStrLn ("Thanks for all the fish, " <> n <> "!")
  pure ExitSuccess


-- * Helpers


-- | Version option parser.
infoOptVersion :: OA.Parser (a -> a)
infoOptVersion =
  OA.infoOption Meta.versionString $
    OA.short 'v'
      <> OA.long "version"
      <> OA.help "Show application version and exit"


-- | Header 'OA.InfoMod'.
infoModHeader :: OA.InfoMod a
infoModHeader =
  OA.header (T.unpack (Meta.name <> " - " <> Meta.title <> " v" <> Meta.versionText))


-- | Footer 'OA.InfoMod'.
infoModFooter :: OA.InfoMod a
infoModFooter =
  OA.footer "See <https://github.com/vst/opsops> for help and feedback."


-- | Tests a parser with given arguments.
runParserTest :: OA.Parser a -> [String] -> OA.ParserResult a
runParserTest parser = OA.execParserPure (OA.prefs prefs) (OA.info (parser <**> OA.helper) infomod)
  where
    prefs = OA.showHelpOnError <> OA.helpLongEquals <> OA.helpShowGlobals
    infomod = OA.fullDesc <> OA.progDesc "Test Parser" <> OA.header "testparser - especially for doctests"


-- | Tests an IO parser with given arguments.
runParserTestIO :: OA.Parser (IO a) -> [String] -> IO (Either String ())
runParserTestIO p as = case runParserTest p as of
  OA.Success _ -> pure (Right ())
  OA.Failure f -> pure (Left (show f))
  OA.CompletionInvoked _ -> pure (Right ())
