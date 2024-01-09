{-# LANGUAGE OverloadedStrings #-}

-- | This module provides auxiliary definitions for textual values.
module Zamazingo.Text where

import qualified Data.Text as T


-- $setup
--
-- >>> :set -XOverloadedStrings


-- | Like 'show' but produces 'T.Text'.
tshow :: Show a => a -> T.Text
tshow = T.pack . show


-- | Sanitizes a given string by replacing consecutive whitespace with
-- a single space.
--
-- >>> sanitize ""
-- ""
-- >>> sanitize "  "
-- ""
-- >>> sanitize "  a   c   b  "
-- "a c b"
sanitize :: T.Text -> T.Text
sanitize = T.unwords . T.words


-- | Returns a 'Maybe' of non empty string.
--
-- >>> nonempty ""
-- Nothing
-- >>> nonempty " "
-- Just " "
nonempty :: T.Text -> Maybe T.Text
nonempty "" = Nothing
nonempty x = Just x
