module RnSplice where

import GHC.Syntax
import TcRnMonad
import GHC.Data.RdrName
import GHC.Data.Name
import GHC.Data.Name.Set
import GHC.Data.Kind


rnSpliceType :: HsSplice RdrName   -> PostTc Name Kind
             -> RnM (HsType Name, FreeVars)
rnSplicePat  :: HsSplice RdrName   -> RnM ( Either (Pat RdrName) (Pat Name)
                                          , FreeVars )
rnSpliceDecl :: SpliceDecl RdrName -> RnM (SpliceDecl Name, FreeVars)

rnTopSpliceDecls :: HsSplice RdrName -> RnM ([LHsDecl RdrName], FreeVars)
