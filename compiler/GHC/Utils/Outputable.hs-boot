module GHC.Utils.Outputable where

data SDoc

showSDocUnsafe :: SDoc -> String

warnPprTrace :: Bool -> String -> Int -> SDoc -> a -> a

text :: String -> SDoc
