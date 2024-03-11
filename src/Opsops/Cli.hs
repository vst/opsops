{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

-- | This module provides top-level definitions for the CLI program.
module Opsops.Cli where

import Control.Applicative ((<**>), (<|>))
import Control.Monad (join)
import qualified Data.ByteString.Char8 as BC
import Data.String.Interpolate (i)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Yaml as Yaml
import qualified Opsops.Meta
import qualified Opsops.Nix
import qualified Opsops.Render
import qualified Opsops.Spec
import qualified Options.Applicative as OA
import qualified Path.IO as PIO
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
        <> OA.progDesc [i|Visit <#{Opsops.Meta.homepage}> for more information.|]
        <> infoModHeader
        <> infoModFooter


-- * Program


-- | Option parser for top-level commands.
optProgram :: OA.Parser (IO ExitCode)
optProgram =
  commandNormalize
    <|> commandRender
    <|> commandSnippet


-- * Commands


-- ** normalize


-- | Definition for @normalize@ CLI command.
commandNormalize :: OA.Parser (IO ExitCode)
commandNormalize =
  OA.hsubparser (OA.command "normalize" (OA.info parser infomod) <> OA.metavar "normalize")
  where
    infomod =
      OA.fullDesc
        <> infoModHeader
        <> OA.progDesc "Normalize specification"
        <> OA.footer "This command prints the canonical specification."
    parser =
      doNormalize
        <$> OA.strOption (OA.short 'i' <> OA.long "input" <> OA.action "file" <> OA.help "Path to the specification file.")


-- | @normalize@ CLI command program.
doNormalize :: FilePath -> IO ExitCode
doNormalize f = do
  path <- PIO.resolveFile' f
  spec <- Opsops.Spec.readSpecFile path
  BC.putStrLn (Yaml.encode spec)
  pure ExitSuccess


-- ** render


-- | Definition for @render@ CLI command.
commandRender :: OA.Parser (IO ExitCode)
commandRender =
  OA.hsubparser (OA.command "render" (OA.info parser infomod) <> OA.metavar "render")
  where
    infomod =
      OA.fullDesc
        <> infoModHeader
        <> OA.progDesc "Render specification"
        <> OA.footer "This command renders the specification into clear secrets."
    parser =
      doRender
        <$> OA.strOption (OA.short 'i' <> OA.long "input" <> OA.action "file" <> OA.help "Path to the specification file.")


-- | @render@ CLI command program.
doRender :: FilePath -> IO ExitCode
doRender f = do
  path <- PIO.resolveFile' f
  spec <- Opsops.Spec.readSpecFile path
  clear <- Opsops.Render.renderSpec spec
  BC.putStrLn (Yaml.encode clear)
  pure ExitSuccess


-- ** snippet


-- | Definition for @snippet@ CLI command.
commandSnippet :: OA.Parser (IO ExitCode)
commandSnippet =
  OA.hsubparser (OA.command "snippet" (OA.info parser infomod) <> OA.metavar "snippet")
  where
    infomod =
      OA.fullDesc
        <> infoModHeader
        <> OA.progDesc "Show snippets"
        <> OA.footer "This command prints snippets."
    parser =
      commandSnippetSopsNix


-- *** sops-nix


-- | Definition for @snippet sops-nix@ CLI command.
commandSnippetSopsNix :: OA.Parser (IO ExitCode)
commandSnippetSopsNix =
  OA.hsubparser (OA.command "sops-nix" (OA.info parser infomod) <> OA.metavar "sops-nix")
  where
    infomod =
      OA.fullDesc
        <> infoModHeader
        <> OA.progDesc "Show sops-nix snippet"
        <> OA.footer "This command prints sample sops-nix snippet."
    parser =
      doSnippetSopsNix
        <$> OA.strOption (OA.short 'i' <> OA.long "input" <> OA.action "file" <> OA.help "Path to the specification file.")
        <*> OA.optional (OA.strOption (OA.short 'p' <> OA.long "prefix" <> OA.help "Optional prefix for sops-nix path."))


-- | @snippet sops-nix@ CLI command program.
doSnippetSopsNix :: FilePath -> Maybe T.Text -> IO ExitCode
doSnippetSopsNix f mP = do
  path <- PIO.resolveFile' f
  spec <- Opsops.Spec.readSpecFile path
  TIO.putStrLn (Opsops.Nix.sopsNixSnippet mP spec)
  pure ExitSuccess


-- * Helpers


-- | Version option parser.
infoOptVersion :: OA.Parser (a -> a)
infoOptVersion =
  OA.infoOption Opsops.Meta.versionString $
    OA.short 'v'
      <> OA.long "version"
      <> OA.help "Show application version and exit"


-- | Header 'OA.InfoMod'.
infoModHeader :: OA.InfoMod a
infoModHeader =
  OA.header (T.unpack (Opsops.Meta.name <> " - " <> Opsops.Meta.title <> " v" <> Opsops.Meta.versionText))


-- | Footer 'OA.InfoMod'.
infoModFooter :: OA.InfoMod a
infoModFooter =
  OA.footer [i|See <#{Opsops.Meta.homepage}> for help and feedback.|]


-- | Tests a parser with given arguments.
runParserTest :: OA.Parser a -> [String] -> OA.ParserResult a
runParserTest parser =
  OA.execParserPure (OA.prefs prefs) (OA.info (parser <**> OA.helper) infomod)
  where
    prefs = OA.showHelpOnError <> OA.helpLongEquals <> OA.helpShowGlobals
    infomod = OA.fullDesc <> OA.progDesc "Test Parser" <> OA.header "testparser - especially for doctests"


-- | Tests an IO parser with given arguments.
runParserTestIO :: OA.Parser (IO a) -> [String] -> IO (Either String ())
runParserTestIO p as =
  case runParserTest p as of
    OA.Success _ -> pure (Right ())
    OA.Failure f -> pure (Left (show f))
    OA.CompletionInvoked _ -> pure (Right ())
