module ServantSerf.Type.Config where

import qualified Control.Monad as Monad
import qualified Control.Monad.Catch as Exception
import qualified ServantSerf.Exception.InvalidDepth as InvalidDepth
import qualified ServantSerf.Exception.InvalidModuleName as InvalidModuleName
import qualified ServantSerf.Type.Depth as Depth
import qualified ServantSerf.Type.Flag as Flag
import qualified ServantSerf.Type.ModuleName as ModuleName

data Config = Config
  { apiName :: String,
    depth :: Depth.Depth,
    excludeSuffix :: String,
    help :: Bool,
    moduleName :: Maybe ModuleName.ModuleName,
    serverName :: String,
    version :: Bool
  }
  deriving (Eq, Show)

fromFlags :: (Foldable t, Exception.MonadThrow m) => t Flag.Flag -> m Config
fromFlags = Monad.foldM applyFlag initial

applyFlag :: (Exception.MonadThrow m) => Config -> Flag.Flag -> m Config
applyFlag config flag = case flag of
  Flag.ApiName x -> pure config {apiName = x}
  Flag.Depth x -> case Depth.fromString x of
    Nothing -> Exception.throwM $ InvalidDepth.InvalidDepth x
    Just y -> pure config {depth = y}
  Flag.ExcludeSuffix x -> pure config {excludeSuffix = x}
  Flag.Help -> pure config {help = True}
  Flag.ModuleName x -> case ModuleName.fromString x of
    Nothing -> Exception.throwM $ InvalidModuleName.InvalidModuleName x
    Just y -> pure config {moduleName = Just y}
  Flag.ServerName x -> pure config {serverName = x}
  Flag.Version -> pure config {version = True}

initial :: Config
initial =
  Config
    { apiName = "API",
      depth = Depth.Deep,
      excludeSuffix = "",
      help = False,
      moduleName = Nothing,
      serverName = "server",
      version = False
    }
