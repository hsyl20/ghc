module GHC.Data.ConLike where
import {-# SOURCE #-} GHC.Data.DataCon (DataCon)
import {-# SOURCE #-} PatSyn (PatSyn)
import GHC.Data.Name ( Name )

data ConLike = RealDataCon DataCon
             | PatSynCon PatSyn

conLikeName :: ConLike -> Name
