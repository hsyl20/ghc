{-
(c) The University of Glasgow 2006
(c) The GRASP/AQUA Project, Glasgow University, 1992-1998
-}

module GHC.IR.Haskell.TypeSystem.Instance ( tcInstDecls1 ) where

import GHC.IR.Haskell.Syntax
import GHC.IR.Haskell.TypeSystem.Types
import GHC.IR.Haskell.TypeSystem.Environment( InstInfo )
import GHC.IR.Haskell.TypeSystem.Deriving
import GHC.Data.Name

-- We need this because of the mutual recursion
-- between GHC.IR.Haskell.TypeSystem.TypeAndClassDeclaration and GHC.IR.Haskell.TypeSystem.Instance
tcInstDecls1 :: [LInstDecl Name] -> TcM (TcGblEnv, [InstInfo Name], [DerivInfo])
