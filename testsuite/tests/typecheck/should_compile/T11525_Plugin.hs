module T11525_Plugin(plugin) where

import GHC.IR.Haskell.TypeSystem ( TcPlugin(..), TcPluginResult(..) )
import GHC.Plugin.Types ( defaultPlugin, Plugin(..), CommandLineOption )

plugin :: Plugin
plugin = defaultPlugin { tcPlugin = Just . thePlugin }

thePlugin :: [CommandLineOption] -> TcPlugin
thePlugin opts = TcPlugin
  { tcPluginInit  = return ()
  , tcPluginSolve = \_ _ _ _ -> return $ TcPluginOk [] []
  , tcPluginStop  = \_ -> return ()
  }
