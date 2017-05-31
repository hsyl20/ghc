module GHC.Data.ConstructorLike where
import {-# SOURCE #-} GHC.Data.DataConstructor (DataCon)
import {-# SOURCE #-} GHC.Data.PatternSynonym (PatSyn)
import GHC.Data.Name ( Name )

data ConLike = RealDataCon DataCon
             | PatSynCon PatSyn

conLikeName :: ConLike -> Name
