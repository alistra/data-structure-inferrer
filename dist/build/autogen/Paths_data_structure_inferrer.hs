module Paths_data_structure_inferrer (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName
  ) where

import Data.Version (Version(..))
import System.Environment (getEnv)

version :: Version
version = Version {versionBranch = [0,1], versionTags = []}

bindir, libdir, datadir, libexecdir :: FilePath

bindir     = "/home/alistra/.cabal/bin"
libdir     = "/home/alistra/.cabal/lib/data-structure-inferrer-0.1/ghc-7.0.4"
datadir    = "/home/alistra/.cabal/share/data-structure-inferrer-0.1"
libexecdir = "/home/alistra/.cabal/libexec"

getBinDir, getLibDir, getDataDir, getLibexecDir :: IO FilePath
getBinDir = catch (getEnv "data_structure_inferrer_bindir") (\_ -> return bindir)
getLibDir = catch (getEnv "data_structure_inferrer_libdir") (\_ -> return libdir)
getDataDir = catch (getEnv "data_structure_inferrer_datadir") (\_ -> return datadir)
getLibexecDir = catch (getEnv "data_structure_inferrer_libexecdir") (\_ -> return libexecdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
