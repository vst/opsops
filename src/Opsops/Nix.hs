{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

-- | This module provides sops-nix definitions.
module Opsops.Nix where

import qualified Data.List as List
import qualified Data.Map.Strict as Map
import Data.String.Interpolate (i)
import qualified Data.Text as T
import Opsops.Spec (
  SecretNode (..),
  SecretNodes (..),
  Spec (..),
 )


-- | Produces a snippet for including into sops-nix configuration from
-- the given optional path prefix and specification.
sopsNixSnippet
  :: Maybe T.Text
  -- ^ Optional path prefix.
  -> Spec
  -- ^ Specification to generate snippet for.
  -> T.Text
sopsNixSnippet mP =
  T.intercalate "\n" . List.sort . itemsFromNodes mP [] . specSecrets


-- | Produces secrets snippet for a given optional path prefix,
-- (reversed) path so far and secret nodes.
itemsFromNodes
  :: Maybe T.Text
  -- ^ Optional path prefix.
  -> [T.Text]
  -- ^ Path (reversed).
  -> SecretNodes
  -- ^ Secret nodes.
  -> [T.Text]
itemsFromNodes mP p MkSecretNodes {..} =
  concatMap (\(k, v) -> itemsFromNode mP (k : p) v) (Map.toList unSecretNodes)


-- | Produces secrets snippet for a given optional path prefix,
-- (reversed) path so far and secret node.
itemsFromNode
  :: Maybe T.Text
  -- ^ Optional path prefix.
  -> [T.Text]
  -- ^ Path (reversed).
  -> SecretNode
  -- ^ Secret node.
  -> [T.Text]
itemsFromNode mP p (SecretNodeNodes ns) = itemsFromNodes mP p ns
itemsFromNode mP p (SecretNodeSecret _) = [itemFromPath mP p]


-- | Produces single secret snippet for a given optional path prefix
-- and the (reversed) path of the secret as per specification.
--
-- >>> itemFromPath Nothing ["b", "a"]
-- "\"a/b\" = {};"
-- >>> itemFromPath (Just "a") ["c", "b"]
-- "\"a/b/c\" = { key = \"b/c\"; };"
itemFromPath
  :: Maybe T.Text
  -- ^ Optional path prefix.
  -> [T.Text]
  -- ^ Path (reversed).
  -> T.Text
itemFromPath mP p =
  let
    key = T.intercalate "/" (List.reverse p)
   in
    case mP of
      Nothing -> [i|"#{key}" = {};|]
      Just sp -> [i|"#{sp}/#{key}" = { key = "#{key}"; };|]
