module Main where

import GHC
import GHC.Packages
import GHC.Monad
import GHC.Util.Outputable
import System.Environment
import GHC.Config.Flags
import GHC.CoreTypes.Module

main =
  do [libdir] <- getArgs
     _ <- runGhc (Just libdir) $ do
                dflags <- getSessionDynFlags
                setSessionDynFlags dflags
                dflags <- getSessionDynFlags
                liftIO $ print (mkModuleName "GHC.Util.Outputable"
                                `elem` listVisibleModuleNames dflags)
     _ <- runGhc (Just libdir) $ do
                dflags <- getSessionDynFlags
                setSessionDynFlags (dflags {
                    packageFlags = [ExposePackage "-package ghc"
                                                  (PackageArg "ghc")
                                                  (ModRenaming True [])]
                    })
                dflags <- getSessionDynFlags
                liftIO $ print (mkModuleName "GHC.Util.Outputable"
                                `elem` listVisibleModuleNames dflags)
     return ()
