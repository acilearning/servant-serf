module ServantSerf.Directory where

import qualified ServantSerf.Type.Depth as Depth
import qualified System.Directory as Directory
import qualified System.FilePath as FilePath

list :: Depth.Depth -> FilePath -> IO [FilePath]
list depth = case depth of
  Depth.Deep -> listDeep
  Depth.Shallow -> listShallow

listDeep :: FilePath -> IO [FilePath]
listDeep directory = do
  entries <- listShallow directory
  concat <$> mapM listDeepHelper entries

listDeepHelper :: FilePath -> IO [FilePath]
listDeepHelper entry = do
  isDirectory <- Directory.doesDirectoryExist entry
  if isDirectory then listDeep entry else pure [entry]

listShallow :: FilePath -> IO [FilePath]
listShallow directory = do
  entries <- Directory.listDirectory directory
  pure $ fmap (FilePath.combine directory) entries
