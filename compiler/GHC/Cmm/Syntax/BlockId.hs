{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

{- BlockId module should probably go away completely, being superseded by Label -}
module GHC.Cmm.Syntax.BlockId
  ( BlockId, mkBlockId -- ToDo: BlockId should be abstract, but it isn't yet
  , newBlockId
  , blockLbl, infoTblLbl
  ) where

import GHC.Types.CLabel
import GHC.Types.Id.Info
import GHC.Types.Name
import GHC.Types.Unique
import GHC.Data.UniqueSupply

import GHC.Cmm.Dataflow.Label (Label, uniqueToLbl)
import GHC.Cmm.Dataflow.Unique (intToUnique)

----------------------------------------------------------------
--- Block Ids, their environments, and their sets

{- Note [Unique BlockId]
~~~~~~~~~~~~~~~~~~~~~~~~
Although a 'BlockId' is a local label, for reasons of implementation,
'BlockId's must be unique within an entire compilation unit.  The reason
is that each local label is mapped to an assembly-language label, and in
most assembly languages allow, a label is visible throughout the entire
compilation unit in which it appears.
-}

type BlockId = Label

mkBlockId :: Unique -> BlockId
mkBlockId unique = uniqueToLbl $ intToUnique $ getKey unique

newBlockId :: MonadUnique m => m BlockId
newBlockId = mkBlockId <$> getUniqueM

blockLbl :: BlockId -> CLabel
blockLbl label = mkEntryLabel (mkFCallName (getUnique label) "block") NoCafRefs

infoTblLbl :: BlockId -> CLabel
infoTblLbl label = mkInfoTableLabel (mkFCallName (getUnique label) "block") NoCafRefs
