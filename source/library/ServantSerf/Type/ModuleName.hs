module ServantSerf.Type.ModuleName where

import qualified Data.List as List
import qualified Data.Maybe as Maybe
import qualified Distribution.ModuleName as Cabal
import qualified Distribution.Text as Cabal
import qualified System.FilePath as FilePath

newtype ModuleName
    = ModuleName Cabal.ModuleName
    deriving (Eq, Ord, Show)

toString :: ModuleName -> String
toString (ModuleName x) = Cabal.display x

fromString :: String -> Maybe ModuleName
fromString = fmap ModuleName . Cabal.simpleParse

fromFilePath :: FilePath -> Maybe ModuleName
fromFilePath =
  Maybe.listToMaybe
    . Maybe.mapMaybe (fromString . List.intercalate ".")
    . List.tails
    . FilePath.splitDirectories
    . FilePath.dropExtensions
