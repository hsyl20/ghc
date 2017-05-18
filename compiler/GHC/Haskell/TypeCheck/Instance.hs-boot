{-
(c) The University of Glasgow 2006
(c) The GRASP/AQUA Project, Glasgow University, 1992-1998
-}

module GHC.Haskell.TypeCheck.Instance ( tcInstDecls1 ) where

import GHC.Haskell.Syntax
import GHC.Haskell.TypeCheck.Util
import GHC.Haskell.TypeCheck.Environment( InstInfo )
import GHC.Haskell.TypeCheck.Deriving
import GHC.Haskell.Syntax.Extension       ( GhcRn )

-- We need this because of the mutual recursion
-- between GHC.Haskell.TypeCheck.TypeDecl and
-- GHC.Haskell.TypeCheck.Instance
tcInstDecls1 :: [LInstDecl GhcRn]
             -> TcM (TcGblEnv, [InstInfo GhcRn], [DerivInfo])
