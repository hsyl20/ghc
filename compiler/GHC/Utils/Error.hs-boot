module GHC.Utils.Error where

import GHC.Utils.Outputable (SDoc, PrintUnqualified )
import GHC.Data.SrcLoc (SrcSpan)
import GHC.Utils.Json
import {-# SOURCE #-} GHC.Config.Flags ( DynFlags, DumpFlag )

data Severity
  = SevOutput
  | SevFatal
  | SevInteractive
  | SevDump
  | SevInfo
  | SevWarning
  | SevError


type MsgDoc = SDoc

mkLocMessage :: Severity -> SrcSpan -> MsgDoc -> MsgDoc
mkLocMessageAnn :: Maybe String -> Severity -> SrcSpan -> MsgDoc -> MsgDoc
getCaretDiagnostic :: Severity -> SrcSpan -> IO MsgDoc
dumpSDoc :: DynFlags -> PrintUnqualified -> DumpFlag -> String -> SDoc -> IO ()

instance ToJson Severity
