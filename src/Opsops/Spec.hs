{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | This module provides data definitions for and functions to work
-- with "opsops" specification.
module Opsops.Spec where

import Control.Applicative ((<|>))
import Control.Monad.Catch (MonadThrow (throwM))
import Control.Monad.IO.Class (MonadIO (..))
import Data.Aeson ((.!=), (.:), (.:?), (.=))
import qualified Data.Aeson as Aeson
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Data.Yaml as Yaml
import qualified Path as P


-- $setup
--
-- >>> :set -XOverloadedStrings
-- >>> :set -XTypeApplications


-- * Definitions


-- | Data definition for "opsops" specification.
newtype Spec = Spec
  { specSecrets :: SecretNodes
  }
  deriving (Eq, Ord, Show)


-- | 'Aeson.FromJSON' instance for 'Spec'.
instance Aeson.FromJSON Spec where
  parseJSON = Aeson.withObject "Spec" $ \o -> Spec <$> o .: "secrets"


-- | 'Aeson.ToJSON' instance for 'Spec'.
instance Aeson.ToJSON Spec where
  toJSON Spec {..} =
    Aeson.object
      [ "secrets" .= specSecrets
      ]


-- | Data definition for secret nodes.
newtype SecretNodes = MkSecretNodes
  { unSecretNodes :: Map.Map T.Text SecretNode
  }
  deriving (Eq, Ord, Show)


-- | 'Aeson.FromJSON' instance for 'SecretNodes'.
instance Aeson.FromJSON SecretNodes where
  parseJSON = fmap MkSecretNodes . Aeson.parseJSON


-- | 'Aeson.ToJSON' instance for 'SecretNodes'.
instance Aeson.ToJSON SecretNodes where
  toJSON (MkSecretNodes v) = Aeson.toJSON v


-- | Data definition for a secret node.
--
-- It can be a container for more secret nodes or a secret.
data SecretNode
  = SecretNodeNodes SecretNodes
  | SecretNodeSecret Secret
  deriving (Eq, Ord, Show)


-- | 'Aeson.FromJSON' instance for 'SecretNode'.
instance Aeson.FromJSON SecretNode where
  parseJSON v = (SecretNodeSecret <$> Aeson.parseJSON v) <|> (SecretNodeNodes <$> Aeson.parseJSON v)


-- | 'Aeson.ToJSON' instance for 'SecretNode'.
instance Aeson.ToJSON SecretNode where
  toJSON (SecretNodeNodes ns) = Aeson.toJSON ns
  toJSON (SecretNodeSecret s) = Aeson.toJSON s


-- | Data definition for secret specifications.
data Secret
  = SecretProcess Process
  | SecretScript Script
  | SecretOp Op
  | SecretOpRead OpRead
  deriving (Eq, Ord, Show)


-- | 'Aeson.FromJSON' instance for 'Secret'.
instance Aeson.FromJSON Secret where
  parseJSON = Aeson.withObject "Secret" $ \o -> do
    ctype <- o .: "type"
    case ctype of
      "process" -> o .: "value" >>= fmap SecretProcess . Aeson.parseJSON
      "script" -> o .: "value" >>= fmap SecretScript . Aeson.parseJSON
      "op" -> o .: "value" >>= fmap SecretOp . Aeson.parseJSON
      "op-read" -> o .: "value" >>= fmap SecretOpRead . Aeson.parseJSON
      _ -> fail ("Unknown secret type: " <> T.unpack ctype)


-- | 'Aeson.ToJSON' instance for 'Secret'.
instance Aeson.ToJSON Secret where
  toJSON v =
    case v of
      SecretProcess vx -> pack "process" vx
      SecretScript vx -> pack "script" vx
      SecretOp vx -> pack "op" vx
      SecretOpRead vx -> pack "op-read" vx
    where
      pack t a = Aeson.object ["type" .= (t :: T.Text), "value" .= a]


-- | Data definition to read secrets from 1Password using @op://@
-- URIs.
data OpRead = OpRead
  { opReadAccount :: !(Maybe T.Text)
  , opReadUri :: !T.Text
  , opReadNewline :: !Bool
  , opReadStrip :: !(Maybe Strip)
  , opReadTrailingNewline :: !(Maybe Newline)
  }
  deriving (Eq, Ord, Show)


-- | 'Aeson.FromJSON' instance for 'OpRead'.
--
-- >>> Aeson.eitherDecode @OpRead "{\"uri\":\"op://Devops/OokahCuZ4fo8ahphie1aiFa0ei/Config/url\"}"
-- Right (OpRead {opReadAccount = Nothing, opReadUri = "op://Devops/OokahCuZ4fo8ahphie1aiFa0ei/Config/url", opReadNewline = False, opReadStrip = Nothing, opReadTrailingNewline = Nothing})
-- >>> Aeson.eitherDecode @OpRead "{\"account\":\"IPAEPH0JI3REE8FICHOOVU4CHA\",\"newline\":false,\"uri\":\"op://Devops/OokahCuZ4fo8ahphie1aiFa0ei/Config/url\"}"
-- Right (OpRead {opReadAccount = Just "IPAEPH0JI3REE8FICHOOVU4CHA", opReadUri = "op://Devops/OokahCuZ4fo8ahphie1aiFa0ei/Config/url", opReadNewline = False, opReadStrip = Nothing, opReadTrailingNewline = Nothing})
instance Aeson.FromJSON OpRead where
  parseJSON = Aeson.withObject "OpRead" $ \o ->
    OpRead
      <$> (o .:? "account")
      <*> (o .: "uri")
      <*> (o .:? "newline" .!= False)
      <*> (o .:? "strip")
      <*> (o .:? "trailingNewline")


-- | 'Aeson.ToJSON' instance for 'OpRead'.
--
-- >>> let opRead1 = OpRead {opReadAccount=Nothing, opReadUri="op://Devops/OokahCuZ4fo8ahphie1aiFa0ei/Config/url", opReadNewline=False, opReadStrip=Nothing, opReadTrailingNewline=Nothing}
-- >>> Aeson.encode opRead1
-- "{\"account\":null,\"newline\":false,\"strip\":null,\"trailingNewline\":null,\"uri\":\"op://Devops/OokahCuZ4fo8ahphie1aiFa0ei/Config/url\"}"
-- >>> _testJsonRoundtrip opRead1
-- True
--
-- >>> let opRead2 = OpRead {opReadAccount=Just "IPAEPH0JI3REE8FICHOOVU4CHA", opReadUri="op://Devops/OokahCuZ4fo8ahphie1aiFa0ei/Config/url", opReadNewline=False, opReadStrip=Nothing, opReadTrailingNewline=Nothing}
-- >>> Aeson.encode opRead2
-- "{\"account\":\"IPAEPH0JI3REE8FICHOOVU4CHA\",\"newline\":false,\"strip\":null,\"trailingNewline\":null,\"uri\":\"op://Devops/OokahCuZ4fo8ahphie1aiFa0ei/Config/url\"}"
-- >>> _testJsonRoundtrip opRead2
-- True
instance Aeson.ToJSON OpRead where
  toJSON OpRead {..} =
    Aeson.object
      [ "account" .= opReadAccount
      , "uri" .= opReadUri
      , "newline" .= opReadNewline
      , "strip" .= opReadStrip
      , "trailingNewline" .= opReadTrailingNewline
      ]


-- | Data definition to read secrets from 1Password items using a more
-- explicit specification encoding.
--
-- An 'Op' is identical to an 'OpRead' but the encoding of field
-- properties are explicit. Under the hood, it is converted to
-- 'OpRead' URIs to work with.
data Op = Op
  { opAccount :: !(Maybe T.Text)
  , opVault :: !T.Text
  , opItem :: !T.Text
  , opSection :: !(Maybe T.Text)
  , opField :: !T.Text
  , opNewline :: !Bool
  , opStrip :: !(Maybe Strip)
  , opTrailingNewline :: !(Maybe Newline)
  }
  deriving (Eq, Ord, Show)


-- | 'Aeson.FromJSON' instance for 'Op'.
--
-- >>> Aeson.eitherDecode @Op "{\"account\":null,\"field\":\"username\",\"item\":\"yies1Ahl4ahqu1afao4nahshoo\",\"newline\":false,\"section\":null,\"vault\":\"Cloud Accounts\"}"
-- Right (Op {opAccount = Nothing, opVault = "Cloud Accounts", opItem = "yies1Ahl4ahqu1afao4nahshoo", opSection = Nothing, opField = "username", opNewline = False, opStrip = Nothing, opTrailingNewline = Nothing})
-- >>> Aeson.eitherDecode @Op "{\"account\":\"PAIT5BAHSH7DAPEING3EEDIE2E\",\"field\":\"token1\",\"item\":\"yies1Ahl4ahqu1afao4nahshoo\",\"newline\":false,\"section\":\"API Tokens\",\"vault\":\"Cloud Accounts\"}"
-- Right (Op {opAccount = Just "PAIT5BAHSH7DAPEING3EEDIE2E", opVault = "Cloud Accounts", opItem = "yies1Ahl4ahqu1afao4nahshoo", opSection = Just "API Tokens", opField = "token1", opNewline = False, opStrip = Nothing, opTrailingNewline = Nothing})
instance Aeson.FromJSON Op where
  parseJSON = Aeson.withObject "Op" $ \o ->
    Op
      <$> (o .:? "account")
      <*> (o .: "vault")
      <*> (o .: "item")
      <*> (o .:? "section")
      <*> (o .: "field")
      <*> (o .:? "newline" .!= False)
      <*> (o .:? "strip")
      <*> (o .:? "trailingNewline")


-- | 'Aeson.ToJSON' instance for 'Op'.
--
-- >>> let op1 = Op {opAccount=Nothing, opVault="Cloud Accounts", opItem="yies1Ahl4ahqu1afao4nahshoo", opSection=Nothing, opField="username", opNewline=False, opStrip=Nothing, opTrailingNewline=Nothing}
-- >>> Aeson.encode op1
-- "{\"account\":null,\"field\":\"username\",\"item\":\"yies1Ahl4ahqu1afao4nahshoo\",\"newline\":false,\"section\":null,\"strip\":null,\"trailingNewline\":null,\"vault\":\"Cloud Accounts\"}"
-- >>> _testJsonRoundtrip op1
-- True
--
-- >>> let op2 = Op {opAccount=Just "PAIT5BAHSH7DAPEING3EEDIE2E", opVault="Cloud Accounts", opItem="yies1Ahl4ahqu1afao4nahshoo", opSection=Just "API Tokens", opField="token1", opNewline=False, opStrip=Nothing, opTrailingNewline=Nothing}
-- >>> Aeson.encode op2
-- "{\"account\":\"PAIT5BAHSH7DAPEING3EEDIE2E\",\"field\":\"token1\",\"item\":\"yies1Ahl4ahqu1afao4nahshoo\",\"newline\":false,\"section\":\"API Tokens\",\"strip\":null,\"trailingNewline\":null,\"vault\":\"Cloud Accounts\"}"
-- >>> _testJsonRoundtrip op2
-- True
instance Aeson.ToJSON Op where
  toJSON Op {..} =
    Aeson.object
      [ "account" .= opAccount
      , "vault" .= opVault
      , "item" .= opItem
      , "section" .= opSection
      , "field" .= opField
      , "newline" .= opNewline
      , "strip" .= opStrip
      , "trailingNewline" .= opTrailingNewline
      ]


-- | Converts an 'Op' into an 'OpRead'.
opToOpRead :: Op -> OpRead
opToOpRead Op {..} =
  OpRead
    { opReadUri = "op://" <> opVault <> "/" <> opItem <> "/" <> maybe "" (<> "/") opSection <> opField
    , opReadNewline = opNewline
    , opReadAccount = opAccount
    , opReadStrip = opStrip
    , opReadTrailingNewline = opTrailingNewline
    }


-- | Data definition for a process that outputs secret of interest.
--
-- A 'Process' is the command (executable with absolute path OR an
-- executable in our @PATH@), optional list of of arguments to the
-- executable and optional list of environment variables to run the
-- process with.
data Process = Process
  { processCommand :: !T.Text -- TODO: Shall we use "P.SomeBase P.File"?
  , processArguments :: ![T.Text]
  , processEnvironment :: !(Map.Map T.Text T.Text)
  , processStrip :: !(Maybe Strip)
  , processTrailingNewline :: !(Maybe Newline)
  }
  deriving (Eq, Ord, Show)


-- | 'Aeson.FromJSON' instance for 'Process'.
--
-- >>> Aeson.eitherDecode @Process "{\"arguments\":[\"auth\",\"token\"],\"command\":\"gh\",\"environment\":{}}"
-- Right (Process {processCommand = "gh", processArguments = ["auth","token"], processEnvironment = fromList [], processStrip = Nothing, processTrailingNewline = Nothing})
-- >>> Aeson.eitherDecode @Process "{\"arguments\":[],\"command\":\"some-secret\",\"environment\":{\"SECRET\":\"THIS\"}}"
-- Right (Process {processCommand = "some-secret", processArguments = [], processEnvironment = fromList [("SECRET","THIS")], processStrip = Nothing, processTrailingNewline = Nothing})
instance Aeson.FromJSON Process where
  parseJSON = Aeson.withObject "Process" $ \o ->
    Process
      <$> (o .: "command")
      <*> (o .:? "arguments" .!= mempty)
      <*> (o .:? "environment" .!= mempty)
      <*> (o .:? "strip")
      <*> (o .:? "trailingNewline")


-- | 'Aeson.ToJSON' instance for 'Process'.
--
-- >>> let process1 = Process {processCommand="gh", processArguments=["auth", "token"], processEnvironment=mempty, processStrip=Nothing, processTrailingNewline=Nothing}
-- >>> Aeson.encode process1
-- "{\"arguments\":[\"auth\",\"token\"],\"command\":\"gh\",\"environment\":{},\"strip\":null,\"trailingNewline\":null}"
-- >>> _testJsonRoundtrip process1
-- True
--
-- >>> let process2 = Process {processCommand="some-secret", processArguments=[], processEnvironment=Map.fromList [("SECRET", "THIS")], processStrip=Nothing, processTrailingNewline=Nothing}
-- >>> Aeson.encode process2
-- "{\"arguments\":[],\"command\":\"some-secret\",\"environment\":{\"SECRET\":\"THIS\"},\"strip\":null,\"trailingNewline\":null}"
-- >>> _testJsonRoundtrip process2
-- True
instance Aeson.ToJSON Process where
  toJSON Process {..} =
    Aeson.object
      [ "command" .= processCommand
      , "arguments" .= processArguments
      , "environment" .= processEnvironment
      , "strip" .= processStrip
      , "trailingNewline" .= processTrailingNewline
      ]


-- | Data definition for a script that outputs secret of interest.
--
-- A 'Script' is an interpreter, optional list of arguments to the
-- interpreter and content to be passed to the interpreter from the
-- standard input.
data Script = Script
  { scriptInterpreter :: !T.Text
  , scriptArguments :: ![T.Text]
  , scriptContent :: !T.Text
  , scriptStrip :: !(Maybe Strip)
  , scriptTrailingNewline :: !(Maybe Newline)
  }
  deriving (Eq, Ord, Show)


-- | 'Aeson.FromJSON' instance for 'Script'.
--
-- >>> Aeson.eitherDecode @Script "{\"content\": \"echo hebele\"}"
-- Right (Script {scriptInterpreter = "bash", scriptArguments = [], scriptContent = "echo hebele", scriptStrip = Nothing, scriptTrailingNewline = Nothing})
-- >>> Aeson.eitherDecode @Script "{\"interpreter\": \"python3\", \"content\": \"print(\\\"hebele\\\")\"}"
-- Right (Script {scriptInterpreter = "python3", scriptArguments = [], scriptContent = "print(\"hebele\")", scriptStrip = Nothing, scriptTrailingNewline = Nothing})
-- >>> Aeson.eitherDecode @Script "{\"interpreter\": \"bash\", \"arguments\": [\"--noprofile\"], \"content\": \"echo hebele\"}"
-- Right (Script {scriptInterpreter = "bash", scriptArguments = ["--noprofile"], scriptContent = "echo hebele", scriptStrip = Nothing, scriptTrailingNewline = Nothing})
instance Aeson.FromJSON Script where
  parseJSON = Aeson.withObject "Script" $ \o ->
    Script
      <$> (o .:? "interpreter" .!= "bash")
      <*> (o .:? "arguments" .!= mempty)
      <*> (o .: "content")
      <*> (o .:? "strip")
      <*> (o .:? "trailingNewline")


-- | 'Aeson.ToJSON' instance for 'Script'.
--
-- >>> let script1 = Script {scriptInterpreter = "bash", scriptArguments = [], scriptContent = "echo hebele", scriptStrip = Nothing, scriptTrailingNewline = Nothing}
-- >>> Aeson.encode script1
-- "{\"arguments\":[],\"content\":\"echo hebele\",\"interpreter\":\"bash\",\"strip\":null,\"trailingNewline\":null}"
-- >>> _testJsonRoundtrip script1
-- True
--
-- >>> let script2 = Script {scriptInterpreter = "python3", scriptArguments = [], scriptContent = "print(\"hebele\")", scriptStrip = Nothing, scriptTrailingNewline = Nothing}
-- >>> Aeson.encode script2
-- "{\"arguments\":[],\"content\":\"print(\\\"hebele\\\")\",\"interpreter\":\"python3\",\"strip\":null,\"trailingNewline\":null}"
-- >>> _testJsonRoundtrip script2
-- True
--
-- >>> let script3 = Script {scriptInterpreter = "bash", scriptArguments = ["--noprofile"], scriptContent = "echo hebele", scriptStrip = Nothing, scriptTrailingNewline = Nothing}
-- >>> Aeson.encode script3
-- "{\"arguments\":[\"--noprofile\"],\"content\":\"echo hebele\",\"interpreter\":\"bash\",\"strip\":null,\"trailingNewline\":null}"
-- >>> _testJsonRoundtrip script3
-- True
instance Aeson.ToJSON Script where
  toJSON Script {..} =
    Aeson.object
      [ "interpreter" .= scriptInterpreter
      , "arguments" .= scriptArguments
      , "content" .= scriptContent
      , "strip" .= scriptStrip
      , "trailingNewline" .= scriptTrailingNewline
      ]


-- | Data definition for whitespace-trimming options.
data Strip
  = StripLeft
  | StripRight
  | StripBoth
  deriving (Eq, Ord, Show)


-- | 'Aeson.FromJSON' instance for 'Strip'.
--
-- >>> Aeson.eitherDecode @Strip "\"left\""
-- Right StripLeft
-- >>> Aeson.eitherDecode @Strip "\"right\""
-- Right StripRight
-- >>> Aeson.eitherDecode @Strip "\"both\""
-- Right StripBoth
-- >>> Aeson.eitherDecode @Strip "\"unknown\""
-- Left "Error in $: Unknown strip value: unknown"
instance Aeson.FromJSON Strip where
  parseJSON = Aeson.withText "Strip" $ \t -> case t of
    "left" -> pure StripLeft
    "right" -> pure StripRight
    "both" -> pure StripBoth
    _ -> fail ("Unknown strip value: " <> T.unpack t)


-- | 'Aeson.ToJSON' instance for 'Strip'.
--
-- >>> Aeson.encode StripLeft
-- "\"left\""
-- >>> Aeson.encode StripRight
-- "\"right\""
-- >>> Aeson.encode StripBoth
-- "\"both\""
instance Aeson.ToJSON Strip where
  toJSON v = case v of
    StripLeft -> "left"
    StripRight -> "right"
    StripBoth -> "both"


-- | Data definition for newline options.
data Newline
  = NewlineLf
  | NewlineCrlf
  deriving (Eq, Ord, Show)


-- | 'Aeson.FromJSON' instance for 'Newline'.
--
-- >>> Aeson.eitherDecode @Newline "\"lf\""
-- Right NewlineLf
-- >>> Aeson.eitherDecode @Newline "\"crlf\""
-- Right NewlineCrlf
-- >>> Aeson.eitherDecode @Newline "\"unknown\""
-- Left "Error in $: Unknown strip value: unknown"
instance Aeson.FromJSON Newline where
  parseJSON = Aeson.withText "Newline" $ \t -> case t of
    "lf" -> pure NewlineLf
    "crlf" -> pure NewlineCrlf
    _ -> fail ("Unknown strip value: " <> T.unpack t)


-- | 'Aeson.ToJSON' instance for 'Newline'.
--
-- >>> Aeson.encode NewlineLf
-- "\"lf\""
-- >>> Aeson.encode NewlineCrlf
-- "\"crlf\""
instance Aeson.ToJSON Newline where
  toJSON v = case v of
    NewlineLf -> "lf"
    NewlineCrlf -> "crlf"


-- * Readers


-- | Attempts to read and return the 'Spec' from the given file.
readSpecFile
  :: MonadIO m
  => MonadThrow m
  => P.Path P.Abs P.File
  -> m Spec
readSpecFile p = do
  eSpec <- liftIO (Yaml.decodeFileEither (P.toFilePath p))
  either throwM pure eSpec -- TODO: throw more meaningful exception.


-- * Helpers


-- ** Testing


-- | Tests JSON encode-decode roundtrip.
_testJsonRoundtrip
  :: Aeson.FromJSON a
  => Aeson.ToJSON a
  => Eq a
  => a
  -> Bool
_testJsonRoundtrip a =
  Aeson.decode (Aeson.encode a) == Just a
