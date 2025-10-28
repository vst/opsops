{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

-- | This module provides project metadata information definitions.
module Opsops.Meta where

import Data.Aeson (ToJSON (toEncoding))
import qualified Data.Aeson as Aeson
import Data.Maybe (fromMaybe)
import Data.String.Interpolate (__i)
import qualified Data.Text as T
import qualified Data.Time as Time
import Data.Version (Version, showVersion)
import qualified GitHash as Githash
import qualified Language.Haskell.TH as TH
import qualified Paths_opsops as Paths
import qualified System.Info


-- | Application name.
--
-- >>> name
-- "opsops"
name :: T.Text
name = "opsops"


-- | Application title.
--
-- >>> title
-- "SOPS(-Nix) Goodies"
title :: T.Text
title = "SOPS(-Nix) Goodies"


-- | Application version.
--
-- > version
-- Version {versionBranch = [0,0,0], versionTags = []}
version :: Version
version = Paths.version


-- | Application version as a 'String' value.
--
-- > versionString
-- "0.0.0"
versionString :: String
versionString = showVersion version


-- | Application version as a 'T.Text' value.
--
-- > versionText
-- "0.0.0"
versionText :: T.Text
versionText = T.pack versionString


-- | Website homepage URL.
--
-- >>> homepage
-- "https://github.com/vst/opsops"
homepage :: T.Text
homepage = "https://github.com/vst/opsops"


-- | Data definition for build information.
data BuildInfo = BuildInfo
  { _buildInfoName :: !T.Text
  , _buildInfoTitle :: !T.Text
  , _buildInfoVersion :: !T.Text
  , _buildInfoTimestamp :: !T.Text
  , _buildInfoGitTag :: !(Maybe T.Text)
  , _buildInfoGitHash :: !(Maybe T.Text)
  , _buildInfoCompilerName :: !T.Text
  , _buildInfoCompilerVersion :: !T.Text
  }
  deriving (Eq, Show)


instance Aeson.ToJSON BuildInfo where
  toJSON BuildInfo {..} =
    Aeson.object
      [ "name" Aeson..= _buildInfoName
      , "title" Aeson..= _buildInfoTitle
      , "version" Aeson..= _buildInfoVersion
      , "timestamp" Aeson..= _buildInfoTimestamp
      , "gitTag" Aeson..= _buildInfoGitTag
      , "gitHash" Aeson..= _buildInfoGitHash
      , "compilerName" Aeson..= _buildInfoCompilerName
      , "compilerVersion" Aeson..= _buildInfoCompilerVersion
      ]


  toEncoding BuildInfo {..} =
    Aeson.pairs
      ( "name" Aeson..= _buildInfoName
          <> "title" Aeson..= _buildInfoTitle
          <> "version" Aeson..= _buildInfoVersion
          <> "timestamp" Aeson..= _buildInfoTimestamp
          <> "gitTag" Aeson..= _buildInfoGitTag
          <> "gitHash" Aeson..= _buildInfoGitHash
          <> "compilerName" Aeson..= _buildInfoCompilerName
          <> "compilerVersion" Aeson..= _buildInfoCompilerVersion
      )


-- | Returns the build information generated as per compile time.
buildInfo :: BuildInfo
buildInfo =
  BuildInfo
    { _buildInfoName = name
    , _buildInfoTitle = title
    , _buildInfoVersion = versionText
    , _buildInfoTimestamp = $(TH.stringE . Time.formatTime Time.defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ" =<< TH.runIO Time.getCurrentTime)
    , _buildInfoGitTag = either (const Nothing) (Just . T.pack . Githash.giTag) gitInfo
    , _buildInfoGitHash = either (const Nothing) (Just . T.pack . Githash.giHash) gitInfo
    , _buildInfoCompilerName = T.pack System.Info.compilerName
    , _buildInfoCompilerVersion = T.pack (showVersion System.Info.fullCompilerVersion)
    }


-- | Builds a pretty text from a given 'BuildInfo' value.
prettyBuildInfo :: BuildInfo -> T.Text
prettyBuildInfo BuildInfo {..} =
  [__i|
Name: #{_buildInfoName}
Title: #{_buildInfoTitle}
Version: #{_buildInfoVersion}
Timestamp: #{_buildInfoTimestamp}
Git Tag: #{fromMaybe "N/A" _buildInfoGitTag}
Git Hash: #{fromMaybe "N/A" _buildInfoGitHash}
Compiler Name: #{_buildInfoCompilerName}
Compiler Version: #{_buildInfoCompilerVersion}
|]


-- | Git information if any.
gitInfo :: Either String Githash.GitInfo
gitInfo = $$Githash.tGitInfoCwdTry
