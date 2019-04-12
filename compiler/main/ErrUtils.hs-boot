{-# LANGUAGE RankNTypes #-}

module ErrUtils where

import GhcPrelude
import Outputable (SDoc, PprStyle )
import SrcLoc (SrcSpan)
import Json
import {-# SOURCE #-} DynFlags ( DynFlags )

type DumpAction = DynFlags -> PprStyle -> DumpOptions -> String
                  -> DumpFormat -> SDoc -> IO ()

type TraceAction = forall a. DynFlags -> String -> SDoc -> a -> a

data DumpOptions = DumpOptions
   { dumpMandatoryFile :: Bool
   , dumpSuffix        :: String
   }

data DumpFormat
  = FormatHaskell
  | FormatCore
  | FormatSTG
  | FormatByteCode
  | FormatCMM
  | FormatASM
  | FormatC
  | FormatLLVM
  | FormatText

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
defaultDumpAction :: DumpAction
defaultTraceAction :: TraceAction

instance ToJson Severity
