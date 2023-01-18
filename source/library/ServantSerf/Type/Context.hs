module ServantSerf.Type.Context where

import qualified Control.Monad.Catch as Exception
import qualified ServantSerf.Exception.ExtraArgument as ExtraArgument
import qualified ServantSerf.Exception.InvalidOption as InvalidOption
import qualified ServantSerf.Exception.MissingArgument as MissingArgument
import qualified ServantSerf.Exception.UnknownOption as UnknownOption
import qualified ServantSerf.Type.Config as Config
import qualified ServantSerf.Type.Flag as Flag
import qualified System.Console.GetOpt as Console

data Context = Context
  { config :: Config.Config,
    input :: FilePath,
    output :: FilePath,
    source :: FilePath
  }
  deriving (Eq, Show)

fromArguments :: Exception.MonadThrow m => [String] -> m Context
fromArguments arguments = do
  let (fs, as, us, is) = Console.getOpt' Console.Permute Flag.options arguments
  mapM_ (Exception.throwM . UnknownOption.UnknownOption) us
  mapM_ (Exception.throwM . InvalidOption.InvalidOption) is
  c <- Config.fromFlags fs
  case as of
    [] -> Exception.throwM $ MissingArgument.MissingArgument "SOURCE"
    [_] -> Exception.throwM $ MissingArgument.MissingArgument "INPUT"
    [_, _] -> Exception.throwM $ MissingArgument.MissingArgument "OUTPUT"
    s : i : o : xs -> do
      mapM_ (Exception.throwM . ExtraArgument.ExtraArgument) xs
      pure Context {config = c, input = i, output = o, source = s}
