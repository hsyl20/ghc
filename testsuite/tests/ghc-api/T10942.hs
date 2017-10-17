module Main where

import GHC.Config.Flags
import GHC

import Control.Monad.IO.Class (liftIO)
import System.Environment
import GHC.Haskell.Parser.HeaderInfo
import GHC.Util.Outputable
import GHC.Data.StringBuffer

main :: IO ()
main = do
  [libdir] <- getArgs
  runGhc (Just libdir) $ do
    dflags <- getSessionDynFlags
    let dflags' = dflags `gopt_set` Opt_KeepRawTokenStream
                         `gopt_set` Opt_Haddock
        filename = "T10942_A.hs"
    setSessionDynFlags dflags'
    stringBuffer <- liftIO $ hGetStringBuffer filename
    liftIO $ print (map unLoc (getOptions dflags' stringBuffer filename))
