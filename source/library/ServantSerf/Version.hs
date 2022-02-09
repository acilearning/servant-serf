module ServantSerf.Version where

import qualified Data.Version as Version
import qualified Paths_servant_serf as Package

string :: String
string = Version.showVersion Package.version
