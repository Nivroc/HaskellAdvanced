{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_exercise07 (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

#if defined(VERSION_base)

#if MIN_VERSION_base(4,0,0)
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#else
catchIO :: IO a -> (Exception.Exception -> IO a) -> IO a
#endif

#else
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#endif
catchIO = Exception.catch

version :: Version
version = Version [0,1,0,0] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "C:\\HaskellProj\\haskell-exercises\\07-ConstraintKinds\\.stack-work\\install\\3fe4d10b\\bin"
libdir     = "C:\\HaskellProj\\haskell-exercises\\07-ConstraintKinds\\.stack-work\\install\\3fe4d10b\\lib\\x86_64-windows-ghc-8.10.4\\exercise07-0.1.0.0-1GFEponQs9J3YtlLU2k7y9"
dynlibdir  = "C:\\HaskellProj\\haskell-exercises\\07-ConstraintKinds\\.stack-work\\install\\3fe4d10b\\lib\\x86_64-windows-ghc-8.10.4"
datadir    = "C:\\HaskellProj\\haskell-exercises\\07-ConstraintKinds\\.stack-work\\install\\3fe4d10b\\share\\x86_64-windows-ghc-8.10.4\\exercise07-0.1.0.0"
libexecdir = "C:\\HaskellProj\\haskell-exercises\\07-ConstraintKinds\\.stack-work\\install\\3fe4d10b\\libexec\\x86_64-windows-ghc-8.10.4\\exercise07-0.1.0.0"
sysconfdir = "C:\\HaskellProj\\haskell-exercises\\07-ConstraintKinds\\.stack-work\\install\\3fe4d10b\\etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "exercise07_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "exercise07_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "exercise07_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "exercise07_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "exercise07_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "exercise07_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "\\" ++ name)
