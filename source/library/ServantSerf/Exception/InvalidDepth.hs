module ServantSerf.Exception.InvalidDepth where

import qualified Control.Monad.Catch as Exception

newtype InvalidDepth
  = InvalidDepth String
  deriving (Eq, Show)

instance Exception.Exception InvalidDepth
