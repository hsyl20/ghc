module GHC.Util.Outputable where

import GHC.Prelude

data SDoc

showSDocUnsafe :: SDoc -> String

warnPprTrace :: Bool -> String -> Int -> SDoc -> a -> a

text :: String -> SDoc
