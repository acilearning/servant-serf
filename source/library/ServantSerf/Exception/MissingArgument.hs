module ServantSerf.Exception.MissingArgument where

import qualified Control.Monad.Catch as Exception

newtype MissingArgument
  = MissingArgument String
  deriving (Eq, Show)

instance Exception.Exception MissingArgument
