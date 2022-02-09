module ServantSerf.Main where

import qualified Control.Monad as Monad
import qualified ServantSerf.Directory as Directory
import qualified ServantSerf.Module as Module
import qualified ServantSerf.Type.Config as Config
import qualified ServantSerf.Type.Context as Context
import qualified ServantSerf.Type.Flag as Flag
import qualified ServantSerf.Version as Version
import qualified System.Console.GetOpt as Console
import qualified System.Environment as Environment
import qualified System.Exit as Exit
import qualified System.FilePath as FilePath

defaultMain :: IO ()
defaultMain = do
  name <- Environment.getProgName
  arguments <- Environment.getArgs
  mainWith name arguments

mainWith :: String -> [String] -> IO ()
mainWith name arguments = do
  context <- Context.fromArguments arguments
  Monad.when (Config.help $ Context.config context) $ do
    putStr $ Console.usageInfo (header name) Flag.options
    Exit.exitSuccess
  Monad.when (Config.version $ Context.config context) $ do
    putStrLn Version.string
    Exit.exitSuccess
  files <-
    Directory.list (Config.depth $ Context.config context)
    . FilePath.dropExtension
    $ Context.source context
  writeFile (Context.output context) $ Module.generate context files

header :: String -> String
header name = name <> " SOURCE INPUT OUTPUT"
