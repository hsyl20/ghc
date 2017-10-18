module GHC.Haskell.Rename.Splice where

import GHC.Prelude
import GHC.Syntax
import GHC.TypeCheck.Monad
import GHC.CoreTypes.Name.Set
import GHC.CoreTypes.Kind


rnSpliceType :: HsSplice GhcPs   -> PostTc GhcRn Kind
             -> RnM (HsType GhcRn, FreeVars)
rnSplicePat  :: HsSplice GhcPs   -> RnM ( Either (Pat GhcPs) (Pat GhcRn)
                                          , FreeVars )
rnSpliceDecl :: SpliceDecl GhcPs -> RnM (SpliceDecl GhcRn, FreeVars)

rnTopSpliceDecls :: HsSplice GhcPs -> RnM ([LHsDecl GhcPs], FreeVars)
