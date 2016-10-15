module TcSimplify where

import TcMType   as TcM
import TcRnMonad as TcM
import TcType
import Coercion

tcFulfilConstraint :: PredType -> TcM (Maybe (Coercion,TcType))
