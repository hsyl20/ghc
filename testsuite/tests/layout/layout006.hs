
module M where

-- GHC.IR.Haskell.Parser.Syntax had a piece of code like this

f :: IO ()
f
 | True = do
 let x = ()
     y = ()
 return ()
 | True = return ()

