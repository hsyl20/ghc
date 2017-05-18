
module M where

-- GHC's GHC.IR.Haskell.Parser.Syntax.lhs had a piece of code like this

f :: IO ()
f
 | True = do
 let x = ()
     y = ()
 return ()
 | True = return ()

