{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Options 
  ( Options(..)
  , options
  , Options.Applicative.execParser
  , Config(..)
  , Pattern'
  ) where

import Data.Bifunctor
import Data.Text (Text)
import Options.Applicative
import Regex

data Options = Options
  { originalInputFile :: FilePath
  , inputFile :: FilePath
  , outputFile :: FilePath
  , config :: Config
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
  <*> configParser

configParser :: Parser Config
configParser = Config
  <$> option str
    (  long "package-name"
    <> short 'p'
    )
  <*> option (eitherReader $ first show . parseRegex)
    (  long "is-handler-module"
    <> short 'm'
    )

data Config = Config
  { packageName :: Text
  , isHandlerModule :: Pattern'
  }
