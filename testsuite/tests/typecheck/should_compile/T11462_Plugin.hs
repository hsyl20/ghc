module T11462_Plugin(plugin) where

import GHC.Haskell.TypeCheck ( TcPlugin(..), TcPluginResult(..) )
import GHC.Plugin.Types ( defaultPlugin, Plugin(..), CommandLineOption )

plugin :: Plugin
plugin = defaultPlugin { tcPlugin = Just . thePlugin }

thePlugin :: [CommandLineOption] -> TcPlugin
thePlugin opts = TcPlugin
  { tcPluginInit  = return ()
  , tcPluginSolve = \_ _ _ _ -> return $ TcPluginOk [] []
  , tcPluginStop  = \_ -> return ()
  }
