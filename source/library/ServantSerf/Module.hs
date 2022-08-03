module ServantSerf.Module where

import qualified Data.List as List
import qualified Data.Maybe as Maybe
import qualified ServantSerf.Type.Config as Config
import qualified ServantSerf.Type.Context as Context
import qualified ServantSerf.Type.ModuleName as ModuleName
import qualified System.FilePath as FilePath

generate :: Context.Context -> [FilePath] -> String
generate context files =
  let
    source = Context.source context
    config = Context.config context
    apiName = Config.apiName config
    serverName = Config.serverName config
    moduleName = case Config.moduleName config of
      Nothing ->
        maybe "Main" ModuleName.toString
          . ModuleName.fromFilePath
          $ Context.source context
      Just x -> ModuleName.toString x
    moduleNames =
      fmap ModuleName.toString
        . List.sort
        . Maybe.mapMaybe ModuleName.fromFilePath
        . filter (not . List.isSuffixOf (Config.suffix config))
        $ filter (FilePath.isExtensionOf "hs") files
  in unlines
    [ "{-# LINE 1 " <> show source <> " #-}"
    , "{-# OPTIONS_GHC -w #-}"
    , ""
    , "module " <> moduleName <> " where"
    , ""
    , "import qualified Servant"
    , ""
    , List.intercalate "\n" $ fmap ("import qualified " <>) moduleNames
    , ""
    , "type " <> apiName
    , "\t= " <> if null moduleNames
      then "Servant.EmptyAPI"
      else List.intercalate "\n\tServant.:<|> "
        $ fmap (<> "." <> apiName) moduleNames
    , ""
    , serverName
    , "\t= " <> if null moduleNames
      then "Servant.emptyServer"
      else List.intercalate "\n\tServant.:<|> "
        $ fmap (<> "." <> serverName) moduleNames
    ]
