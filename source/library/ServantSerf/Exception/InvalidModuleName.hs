module ServantSerf.Exception.InvalidModuleName where

import qualified Control.Monad.Catch as Exception

newtype InvalidModuleName
    = InvalidModuleName String
    deriving (Eq, Show)

instance Exception.Exception InvalidModuleName
