{-
(c) The University of Glasgow 2006
(c) The GRASP/AQUA Project, Glasgow University, 1992-1998
-}

module GHC.IR.Haskell.TypeChecker.Instance ( tcInstDecls1 ) where

import GHC.IR.Haskell.Syntax
import GHC.IR.Haskell.TypeChecker.Types
import GHC.IR.Haskell.TypeChecker.Environment( InstInfo )
import GHC.IR.Haskell.TypeChecker.Deriving
import GHC.IR.Haskell.Syntax.Extension ( GhcRn )

-- We need this because of the mutual recursion
-- between GHC.IR.Haskell.TypeChecker.TypeAndClassDeclaration and
-- GHC.IR.Haskell.TypeChecker.Instance
tcInstDecls1 :: [LInstDecl GhcRn]
             -> TcM (TcGblEnv, [InstInfo GhcRn], [DerivInfo])
