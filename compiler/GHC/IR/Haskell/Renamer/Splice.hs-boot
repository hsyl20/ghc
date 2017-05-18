module GHC.IR.Haskell.Renamer.Splice where

import GHC.IR.Haskell.Syntax
import GHC.IR.Haskell.TypeChecker
import GHC.Entity.Name.Set
import GHC.Entity.Kind


rnSpliceType :: HsSplice GhcPs   -> PostTc GhcRn Kind
             -> RnM (HsType GhcRn, FreeVars)
rnSplicePat  :: HsSplice GhcPs   -> RnM ( Either (Pat GhcPs) (Pat GhcRn)
                                          , FreeVars )
rnSpliceDecl :: SpliceDecl GhcPs -> RnM (SpliceDecl GhcRn, FreeVars)

rnTopSpliceDecls :: HsSplice GhcPs -> RnM ([LHsDecl GhcPs], FreeVars)
