{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}

module Main where

import Control.Monad.Except
import Data.Text (Text)
import Options
import Regex
import ApiModule
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (stderr)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Hpack.Config as Hpack

type Preprocessor = ExceptT PreprocessorException IO

data PreprocessorException
  = PackageYamlParseFailure Text
  | NoLibrary
  | EmptyHandlerModules [Text]
  | MissingImports [Text]

logErrLn :: MonadIO m => Text -> m ()
logErrLn text = liftIO $ T.hPutStrLn stderr text

main :: IO ()
main = do
  Options{..} <- execParser options
  (origInputFile : input : output : _) <- getArgs
  contents <- liftIO $ T.readFile input
  apiModule@ApiModule{..} <- case decodeApiModule contents of
    Right apiModule -> pure apiModule
    Left err -> do
      logErrLn $ "Parse error: " <> T.pack err
      exitFailure
  mException <- runExceptT $ mainPP apiModule config
  let
    outputModule = case mException of
      Left exception -> renderException origInputFile moduleName exception
      Right () -> renderApiModule apiModule
  T.writeFile output outputModule

mainPP :: ApiModule -> Config -> Preprocessor ()
mainPP ApiModule{..} Config{..} = do
  -- parse package.yaml
  epackage <- liftIO $ Hpack.readPackageConfig $ Hpack.defaultDecodeOptions
    { Hpack.decodeOptionsProgramName = Hpack.ProgramName $ T.unpack packageName
    }
  package :: Hpack.Package <- fmap Hpack.decodeResultPackage $ case epackage of
    Left e -> throwError $ PackageYamlParseFailure $ T.pack e
    Right p -> pure p
  allModules :: [Text] <- case Hpack.packageLibrary package of
    Nothing -> throwError NoLibrary
    Just section ->
      let lib = Hpack.sectionData section
      in pure $ getAllModules lib
  let handlerModules = filterPattern isHandlerModule allModules
  case handlerModules of
    [] -> throwError $ EmptyHandlerModules allModules
    _ -> do
      let handlerImports = filterPattern isHandlerModule $ fmap getModuleName imports
      case difference handlerModules handlerImports of
        [] -> pure () -- all good, discovered modules are in scope
        xs -> throwError $ MissingImports xs

getAllModules :: Hpack.Library -> [Text]
getAllModules lib = T.pack . unModule <$> Hpack.libraryExposedModules lib <> Hpack.libraryOtherModules lib

#if MIN_VERSION_hpack(0, 34, 3)
unModule :: Hpack.Module -> String
unModule = Hpack.unModule
#else
unModule :: String -> String
unModule = id
#endif

renderException :: String -> Module -> PreprocessorException -> Text
renderException origInputFile (Module modName) exception = case exception of
  PackageYamlParseFailure errMsg ->
    failModule modName ["Failed to parse package.yaml. Error message: " <> errMsg]
  NoLibrary -> failModule modName ["No library exists in package.yaml"]
  EmptyHandlerModules allModules ->
    failModule modName ["empty handlerModules. allModules = " <> T.pack (show allModules)]
  MissingImports unimported -> do
    let
      makeImportStatement :: Text -> Text
      makeImportStatement x = "import " <> x
      importStatements = fmap makeImportStatement unimported
    failModule modName
      $ [ "Missing handler imports. Consider adding the following imports to the file "
          <> T.pack origInputFile
        , " or updating the is_handler_module regular expression in your .servant-serf.toml"
        ]
      <> importStatements
