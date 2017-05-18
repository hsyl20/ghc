module GHC.Types.ConLike where
import {-# SOURCE #-} GHC.Types.DataCon (DataCon)
import {-# SOURCE #-} GHC.Types.PatternSynonym (PatSyn)
import GHC.Types.Name ( Name )

data ConLike = RealDataCon DataCon
             | PatSynCon PatSyn

conLikeName :: ConLike -> Name
