{-# LANGUAGE RankNTypes, ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module GHC.IR.Cmm.Transform.Dataflow (
    module Compiler.Hoopl,
    module GHC.IR.Cmm.Transform.Dataflow.Hoopl,
  ) where

import Compiler.Hoopl hiding
  ( (<*>), mkLabel, mkBranch, mkMiddle, mkLast, -- clashes with our MkGraph
    DataflowLattice, OldFact, NewFact, JoinFun,
    fact_bot, fact_join, joinOutFacts, mkFactBase,
    Unique,
    FwdTransfer(..), FwdRewrite(..), FwdPass(..),
    BwdTransfer(..), BwdRewrite(..), BwdPass(..),
    mkFactBase, Fact,
    mkBRewrite3, mkBTransfer3,
    mkFRewrite3, mkFTransfer3,

  )

import GHC.IR.Cmm.Transform.Dataflow.Hoopl
import GHC.Utils.Outputable

instance Outputable LabelSet where
  ppr = ppr . setElems

instance Outputable a => Outputable (LabelMap a) where
  ppr = ppr . mapToList
