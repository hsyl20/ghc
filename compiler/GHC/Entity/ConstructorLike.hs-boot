module GHC.Entity.ConstructorLike where
import {-# SOURCE #-} GHC.Entity.DataConstructor (DataCon)
import {-# SOURCE #-} GHC.Entity.PatternSynonym (PatSyn)
import GHC.Entity.Name ( Name )

data ConLike = RealDataCon DataCon
             | PatSynCon PatSyn

conLikeName :: ConLike -> Name
