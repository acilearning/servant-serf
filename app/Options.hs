{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Options 
  ( Options(..)
  , options
  , Options.Applicative.execParser
  , Config(..)
  , defaultConfig
  , configCodec
  , Pattern'
  ) where

import Data.Text (Text)
import GHC.Generics (Generic(..))
import Options.Applicative
import Regex
import Toml ((.=))
import qualified Data.Text as T
import qualified Toml

data Options = Options
  { originalInputFile :: FilePath
  , inputFile :: FilePath
  , outputFile :: FilePath
  }

options :: ParserInfo Options
options = info (options' <**> helper)
  (  fullDesc
  <> progDesc "Generates a servant API module"
  <> header "servant-serf"
  )

options' :: Parser Options
options' = Options
  <$> strArgument
    (   metavar "ORIGINAL_INPUT_FILEPATH"
    )
  <*> strArgument
    (   metavar "INPUT_FILEPATH"
    )
  <*> strArgument
    (   metavar "OUTPUT_FILEPATH"
    )

data Config = Config
  { packageName :: Text
  , isHandlerModule :: Pattern'
  } deriving (Generic)

configCodec :: Toml.TomlCodec Config
configCodec = Config
  <$> Toml.text "package_name" .= packageName
  <*> patternCodec "is_handler_module" .= isHandlerModule

defaultConfig :: Config
defaultConfig = Config
  { packageName = "example"
  , isHandlerModule = either (error . T.unpack) id $ parseRegex' ".*Handler.*"
  }