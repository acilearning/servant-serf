module ServantSerf.Exception.ExtraArgument where

import qualified Control.Monad.Catch as Exception

newtype ExtraArgument
  = ExtraArgument String
  deriving (Eq, Show)

instance Exception.Exception ExtraArgument
