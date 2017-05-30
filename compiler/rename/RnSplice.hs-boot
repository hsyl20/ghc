module RnSplice where

import HsSyn
import TcRnMonad
import GHC.Data.RdrName
import GHC.Data.Name
import GHC.Data.Name.Set
import Kind


rnSpliceType :: HsSplice RdrName   -> PostTc Name Kind
             -> RnM (HsType Name, FreeVars)
rnSplicePat  :: HsSplice RdrName   -> RnM ( Either (Pat RdrName) (Pat Name)
                                          , FreeVars )
rnSpliceDecl :: SpliceDecl RdrName -> RnM (SpliceDecl Name, FreeVars)

rnTopSpliceDecls :: HsSplice RdrName -> RnM ([LHsDecl RdrName], FreeVars)
