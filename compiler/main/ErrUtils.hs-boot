module ErrUtils where

import Outputable (SDoc)
import SrcLoc (SrcSpan)

data Severity
  = SevOutput
  | SevFatal
  | SevInteractive
  | SevDump
  | SevTrace
  | SevInfo
  | SevWarning
  | SevError


type MsgDoc = SDoc

mkLocMessage :: Severity -> SrcSpan -> MsgDoc -> MsgDoc
mkLocMessageAnn :: Maybe String -> Severity -> SrcSpan -> MsgDoc -> MsgDoc
