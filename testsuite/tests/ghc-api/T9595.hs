module Main where

import GHC
import GHC.Packages
import GhcMonad
import GHC.Utils.Outputable
import System.Environment
import GHC.Config.Flags
import Module

main =
  do [libdir] <- getArgs
     _ <- runGhc (Just libdir) $ do
                dflags <- getSessionDynFlags
                setSessionDynFlags dflags
                dflags <- getSessionDynFlags
                liftIO $ print (mkModuleName "Outputable" `elem` listVisibleModuleNames dflags)
     _ <- runGhc (Just libdir) $ do
                dflags <- getSessionDynFlags
                setSessionDynFlags (dflags {
                    packageFlags = [ExposePackage "-package ghc"
                                                  (PackageArg "ghc")
                                                  (ModRenaming True [])]
                    })
                dflags <- getSessionDynFlags
                liftIO $ print (mkModuleName "Outputable" `elem` listVisibleModuleNames dflags)
     return ()
