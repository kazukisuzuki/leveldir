module Main (main) where
import System.Directory
import System.Environment (getArgs)
import System.FilePath ((</>))
import Data.Maybe (catMaybes)
import Data.UUID (toString)
import Data.UUID.V4 (nextRandom)

main :: IO ()
main = do
  extractables <- traverse justOneDirectory =<< getArgs
  mapM_  moveEntriesUpper $ catMaybes extractables

justOneDirectory :: String -> IO (Maybe (String, String))
justOneDirectory rootdir = doesDirectoryExist rootdir >>= \case
  False -> pure Nothing
  _     -> listDirectory rootdir >>= \case
    [subdir] -> pure $ Just (rootdir, rootdir </> subdir)
    _        -> pure Nothing

moveEntriesUpper :: (String, String) -> IO ()
moveEntriesUpper (parent, child) = do
  entries <- listDirectory child
  uuid <- nextRandom
  let beforePaths   = (child  </>)                  <$> entries
  let temporalPaths = (parent </>) . (++ show uuid) <$> entries
  let afterPaths    = (parent </>)                  <$> entries
  let mappedEntries = zip3 beforePaths temporalPaths afterPaths

  mapM_ moveTmp mappedEntries
  mapM_ moveFin mappedEntries
  removeDirectory child

  where
    moveTmp :: (String, String, String) -> IO ()
    moveTmp (before, tmp, _) = renamePath before tmp

    moveFin :: (String, String, String) -> IO ()
    moveFin (_, tmp, after) = renamePath tmp after
