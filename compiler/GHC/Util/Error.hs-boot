module GHC.Util.Error where

import GHC.Prelude
import GHC.Util.Outputable (SDoc, PrintUnqualified )
import GHC.CoreTypes.SrcLoc    (SrcSpan)
import GHC.Util.Json
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
