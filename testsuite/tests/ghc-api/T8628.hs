module Main where

import System.IO
import GHC.Config.Flags
import GHC
import GHC.Utils.Exception
import GHC.Data.Module
import GHC.Data.FastString
import GHC.Utils.Monad
import GHC.Utils.Outputable
import GHC.Data.Bag (filterBag,isEmptyBag)
import System.Directory (removeFile)
import System.Environment( getArgs )
import GHC.Builtin.Names

main :: IO()
main
  = do  [libdir] <- getArgs
        ok <- runGhc (Just libdir) $ do
          dflags <- getSessionDynFlags
          setSessionDynFlags dflags
          liftIO (setUnsafeGlobalDynFlags dflags)

          setContext [ IIDecl (simpleImportDecl pRELUDE_NAME)
                     , IIDecl (simpleImportDecl (mkModuleNameFS (fsLit "System.IO")))]
          runDecls "data X = Y ()"
          execStmt "print True" execOptions
          gtry $ execStmt "print (Y ())" execOptions :: GhcMonad m => m (Either SomeException ExecResult)
          runDecls "data X = Y () deriving Show"
          _ <- dynCompileExpr "'x'"
          execStmt "print (Y ())" execOptions
          execStmt "System.IO.hFlush System.IO.stdout" execOptions
        print "done"
