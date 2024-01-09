{-# LANGUAGE OverloadedStrings #-}

-- | This module provides project metadata information definitions.
module Hebele.Meta where

import qualified Data.Text as T
import Data.Version (Version, showVersion)
import qualified Paths_haskell_template_hebele as Paths


-- | Application name.
--
-- >>> name
-- "haskell-template-hebele"
name :: T.Text
name = "haskell-template-hebele"


-- | Application title.
--
-- >>> title
-- "Haskell Project Template"
title :: T.Text
title = "Haskell Project Template"


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
