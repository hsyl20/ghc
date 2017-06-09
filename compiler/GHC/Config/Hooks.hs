-- \section[Hooks]{Low level API hooks}

-- NB: this module is SOURCE-imported by DynFlags, and should primarily
--     refer to *types*, rather than *code*
-- If you import too much here, then the revolting compiler_stage2_dll0_MODULES
-- stuff in compiler/ghc.mk makes DynFlags link to too much stuff

{-# LANGUAGE CPP #-}
module GHC.Config.Hooks
   ( Hooks
   , emptyHooks
   , lookupHook
   , getHooked
     -- the hooks:
   , dsForeignsHook
   , tcForeignImportsHook
   , tcForeignExportsHook
   , hscFrontendHook
   , hscCompileCoreExprHook
   , ghcPrimIfaceHook
   , runPhaseHook
   , runMetaHook
   , linkHook
   , runRnSpliceHook
   , getValueSafelyHook
   , createIservProcessHook
   )
where

import GHC.Config.Flags
import GHC.Data.Name
import GHC.Program.Driver.Pipeline.Monad
import GHC.Types
import GHC.IR.Haskell.Declaration
import GHC.IR.Haskell.Binding
import GHC.IR.Haskell.Expression
import GHC.Data.Tree.OrdList
import GHC.Data.Id
import GHC.IR.Haskell.TypeSystem.Types
import GHC.Data.Bag
import GHC.Data.RdrName
import GHC.IR.Core.Syntax
import GHCi.RemoteTypes
import GHC.Data.SrcLoc
import GHC.Data.Type
import System.Process
import GHC.Data.BasicTypes

import Data.Maybe

{-
************************************************************************
*                                                                      *
\subsection{Hooks}
*                                                                      *
************************************************************************
-}

-- | Hooks can be used by GHC API clients to replace parts of
--   the compiler pipeline. If a hook is not installed, GHC
--   uses the default built-in behaviour

emptyHooks :: Hooks
emptyHooks = Hooks
  { dsForeignsHook         = Nothing
  , tcForeignImportsHook   = Nothing
  , tcForeignExportsHook   = Nothing
  , hscFrontendHook        = Nothing
  , hscCompileCoreExprHook = Nothing
  , ghcPrimIfaceHook       = Nothing
  , runPhaseHook           = Nothing
  , runMetaHook            = Nothing
  , linkHook               = Nothing
  , runRnSpliceHook        = Nothing
  , getValueSafelyHook     = Nothing
  , createIservProcessHook = Nothing
  }

data Hooks = Hooks
  { dsForeignsHook         :: Maybe ([LForeignDecl Id] -> DsM (ForeignStubs, OrdList (Id, CoreExpr)))
  , tcForeignImportsHook   :: Maybe ([LForeignDecl Name] -> TcM ([Id], [LForeignDecl Id], Bag GlobalRdrElt))
  , tcForeignExportsHook   :: Maybe ([LForeignDecl Name] -> TcM (LHsBinds TcId, [LForeignDecl TcId], Bag GlobalRdrElt))
  , hscFrontendHook        :: Maybe (ModSummary -> Hsc FrontendResult)
  , hscCompileCoreExprHook :: Maybe (HscEnv -> SrcSpan -> CoreExpr -> IO ForeignHValue)
  , ghcPrimIfaceHook       :: Maybe ModIface
  , runPhaseHook           :: Maybe (PhasePlus -> FilePath -> DynFlags -> CompPipeline (PhasePlus, FilePath))
  , runMetaHook            :: Maybe (MetaHook TcM)
  , linkHook               :: Maybe (GhcLink -> DynFlags -> Bool -> HomePackageTable -> IO SuccessFlag)
  , runRnSpliceHook        :: Maybe (HsSplice Name -> RnM (HsSplice Name))
  , getValueSafelyHook     :: Maybe (HscEnv -> Name -> Type -> IO (Maybe HValue))
  , createIservProcessHook :: Maybe (CreateProcess -> IO ProcessHandle)
  }

getHooked :: (Functor f, HasDynFlags f) => (Hooks -> Maybe a) -> a -> f a
getHooked hook def = fmap (lookupHook hook def) getDynFlags

lookupHook :: (Hooks -> Maybe a) -> a -> DynFlags -> a
lookupHook hook def = fromMaybe def . hook . hooks
