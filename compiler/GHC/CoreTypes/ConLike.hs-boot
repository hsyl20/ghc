module GHC.CoreTypes.ConLike where
import {-# SOURCE #-} GHC.CoreTypes.DataCon (DataCon)
import {-# SOURCE #-} GHC.CoreTypes.PatternSynonym (PatSyn)
import GHC.CoreTypes.Name ( Name )

data ConLike = RealDataCon DataCon
             | PatSynCon PatSyn

conLikeName :: ConLike -> Name
