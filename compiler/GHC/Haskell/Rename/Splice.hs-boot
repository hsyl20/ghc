module GHC.Haskell.Rename.Splice where

import GHC.Haskell.Syntax
import GHC.Haskell.TypeCheck
import GHC.Types.Name.Set
import GHC.Types.Kind


rnSpliceType :: HsSplice GhcPs   -> PostTc GhcRn Kind
             -> RnM (HsType GhcRn, FreeVars)
rnSplicePat  :: HsSplice GhcPs   -> RnM ( Either (Pat GhcPs) (Pat GhcRn)
                                          , FreeVars )
rnSpliceDecl :: SpliceDecl GhcPs -> RnM (SpliceDecl GhcRn, FreeVars)

rnTopSpliceDecls :: HsSplice GhcPs -> RnM ([LHsDecl GhcPs], FreeVars)
