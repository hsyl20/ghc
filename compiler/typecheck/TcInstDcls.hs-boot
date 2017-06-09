{-
(c) The University of Glasgow 2006
(c) The GRASP/AQUA Project, Glasgow University, 1992-1998
-}

module TcInstDcls ( tcInstDecls1 ) where

import GHC.IR.Haskell.Syntax
import TcRnTypes
import GHC.IR.Haskell.TypeSystem.Environment( InstInfo )
import GHC.IR.Haskell.TypeSystem.Deriving
import GHC.Data.Name

-- We need this because of the mutual recursion
-- between GHC.IR.Haskell.TypeSystem.TypeAndClassDeclaration and TcInstDcls
tcInstDecls1 :: [LInstDecl Name] -> TcM (TcGblEnv, [InstInfo Name], [DerivInfo])
