{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -Wno-missing-safe-haskell-mode #-}
module Paths_spot_it (
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

bindir     = "/Users/tavishpegram/projects/spot-it/.stack-work/install/x86_64-osx/7303bc00d5cdde2ec6c604b67e8ff8ceb64310eb9120862bf33f448dd195a364/9.0.2/bin"
libdir     = "/Users/tavishpegram/projects/spot-it/.stack-work/install/x86_64-osx/7303bc00d5cdde2ec6c604b67e8ff8ceb64310eb9120862bf33f448dd195a364/9.0.2/lib/x86_64-osx-ghc-9.0.2/spot-it-0.1.0.0-IkqvOf95Bf4dO3XmHCqu4-spot-it"
dynlibdir  = "/Users/tavishpegram/projects/spot-it/.stack-work/install/x86_64-osx/7303bc00d5cdde2ec6c604b67e8ff8ceb64310eb9120862bf33f448dd195a364/9.0.2/lib/x86_64-osx-ghc-9.0.2"
datadir    = "/Users/tavishpegram/projects/spot-it/.stack-work/install/x86_64-osx/7303bc00d5cdde2ec6c604b67e8ff8ceb64310eb9120862bf33f448dd195a364/9.0.2/share/x86_64-osx-ghc-9.0.2/spot-it-0.1.0.0"
libexecdir = "/Users/tavishpegram/projects/spot-it/.stack-work/install/x86_64-osx/7303bc00d5cdde2ec6c604b67e8ff8ceb64310eb9120862bf33f448dd195a364/9.0.2/libexec/x86_64-osx-ghc-9.0.2/spot-it-0.1.0.0"
sysconfdir = "/Users/tavishpegram/projects/spot-it/.stack-work/install/x86_64-osx/7303bc00d5cdde2ec6c604b67e8ff8ceb64310eb9120862bf33f448dd195a364/9.0.2/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "spot_it_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "spot_it_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "spot_it_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "spot_it_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "spot_it_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "spot_it_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
