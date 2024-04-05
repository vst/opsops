{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

-- | This module provides definitions to render "opsops" specification
-- into clear secrets output.
module Opsops.Render where

import Control.Monad.IO.Class (MonadIO (liftIO))
import qualified Data.Aeson as Aeson
import Data.Bifunctor (bimap)
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import Opsops.Spec (
  Newline (..),
  OpRead (..),
  Process (..),
  Script (..),
  Secret (..),
  SecretNode (..),
  SecretNodes (..),
  Spec (..),
  Strip (..),
  opToOpRead,
 )
import System.Environment (getEnvironment)
import System.Exit (ExitCode (..), die)
import qualified System.Process.Typed as TP


-- | Data definition of a forest encoding clear secrets and their
-- paths.
type Forest = Map.Map T.Text Tree


-- | Data definition of a tree encoding clear secrets and their
-- paths.
data Tree
  = TreeSecret T.Text
  | TreeChildren Forest


instance Aeson.ToJSON Tree where
  toJSON (TreeSecret o) = Aeson.toJSON o
  toJSON (TreeChildren o) = Aeson.toJSON o


-- | Renders the given specification into a 'Forest' of clear secrets
-- and their paths.
renderSpec
  :: MonadIO m
  => Spec
  -> m Forest
renderSpec Spec {..} =
  renderSecretNodes specSecrets


-- | Renders given secrets nodes into a 'Forest' of clear secrets and
-- their paths.
renderSecretNodes
  :: MonadIO m
  => SecretNodes
  -> m Forest
renderSecretNodes MkSecretNodes {..} =
  Map.fromList <$> mapM (\(k, v) -> (k,) <$> renderSecretNode v) (Map.toList unSecretNodes)


-- | Renders given secrets node into a 'Tree' of clear secrets and
-- their paths.
renderSecretNode
  :: MonadIO m
  => SecretNode
  -> m Tree
renderSecretNode (SecretNodeNodes nodes) = TreeChildren <$> renderSecretNodes nodes
renderSecretNode (SecretNodeSecret secret) = TreeSecret <$> renderSecret secret


-- | Renders given secret node into a 'Tree' of clear secrets and
-- their paths.
--
-- This is the function that performs the 'MonadIO' operations as per
-- secret type.
renderSecret
  :: MonadIO m
  => Secret
  -> m T.Text
renderSecret (SecretProcess p) = renderProcess p
renderSecret (SecretScript s) = renderScript s
renderSecret (SecretOp o) = renderSecret (SecretOpRead (opToOpRead o))
renderSecret (SecretOpRead OpRead {..}) =
  renderSecret . SecretProcess $
    Process
      { processCommand = "op"
      , processArguments = foldMap (\x -> ["--account", x]) opReadAccount <> ["read"] <> ["--no-newline" | not opReadNewline] <> [opReadUri]
      , processEnvironment = mempty
      , processStrip = opReadStrip
      , processTrailingNewline = opReadTrailingNewline
      }


-- | Attempts to run a 'Process' and return the secret.
renderProcess
  :: MonadIO m
  => Process
  -> m T.Text
renderProcess Process {..} = do
  curenv <- liftIO getEnvironment
  let
    envars = curenv <> fmap (bimap T.unpack T.unpack) (Map.toList processEnvironment)
    command = T.unpack processCommand
    arguments = fmap T.unpack processArguments
    process = TP.setEnv envars (TP.proc command arguments)
    postprocess = maybe id applyNewline processTrailingNewline . maybe id applyStrip processStrip
  (ec, out) <- TP.readProcessStdout process
  case ec of
    ExitFailure _ -> liftIO (die "Error running process. Exiting...")
    ExitSuccess -> pure (postprocess (TL.toStrict (TLE.decodeUtf8 out)))


-- | Attempts to run a 'Script' and return the secret.
renderScript
  :: MonadIO m
  => Script
  -> m T.Text
renderScript Script {..} = do
  let
    interpreter = T.unpack scriptInterpreter
    arguments = fmap T.unpack scriptArguments
    content = TLE.encodeUtf8 (TL.fromStrict scriptContent)
    process = TP.setEnvInherit (TP.setStdin (TP.byteStringInput content) (TP.proc interpreter arguments))
    postprocess = maybe id applyNewline scriptTrailingNewline . maybe id applyStrip scriptStrip
  (ec, out) <- TP.readProcessStdout process
  case ec of
    ExitFailure _ -> liftIO (die "Error running script. Exiting...")
    ExitSuccess -> pure (postprocess (TL.toStrict (TLE.decodeUtf8 out)))


-- | Applies given 'Strip' to the given 'T.Text'.
--
-- >>> applyStrip StripLeft " \t\n hello \t\n "
-- "hello \t\n "
-- >>> applyStrip StripRight " \t\n hello \t\n "
-- " \t\n hello"
-- >>> applyStrip StripBoth " \t\n hello \t\n "
-- "hello"
applyStrip :: Strip -> T.Text -> T.Text
applyStrip StripLeft = T.stripStart
applyStrip StripRight = T.stripEnd
applyStrip StripBoth = T.strip


-- | Applies given 'Newline' to the given 'T.Text'.
--
-- >>> applyNewline NewlineLf "hello"
-- "hello\n"
-- >>> applyNewline NewlineCrlf "hello"
-- "hello\r\n"
applyNewline :: Newline -> T.Text -> T.Text
applyNewline NewlineLf = (<> "\n")
applyNewline NewlineCrlf = (<> "\r\n")
