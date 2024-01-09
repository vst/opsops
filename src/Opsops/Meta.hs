{-# LANGUAGE OverloadedStrings #-}

-- | This module provides project metadata information definitions.
module Opsops.Meta where

import qualified Data.Text as T
import Data.Version (Version, showVersion)
import qualified Paths_opsops as Paths


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
