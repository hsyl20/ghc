{-
(c) The University of Glasgow 2006
(c) The GRASP/AQUA Project, Glasgow University, 1992-1998
-}

module GHC.TypeCheck.Instance ( tcInstDecls1 ) where

import GHC.Syntax
import GHC.TypeCheck.Util
import GHC.TypeCheck.Environment( InstInfo )
import GHC.TypeCheck.Deriving
import GHC.Syntax.Extension       ( GhcRn )

-- We need this because of the mutual recursion
-- between GHC.TypeCheck.TypeDecl and
-- GHC.TypeCheck.Instance
tcInstDecls1 :: [LInstDecl GhcRn]
             -> TcM (TcGblEnv, [InstInfo GhcRn], [DerivInfo])
