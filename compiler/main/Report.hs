module Report
   ( Report (..)
   -- ** Core
   , CoreReport (..)
   , endCorePass
   )
where

import CoreSyn   as Core
import PprCore   as Core
import CoreMonad as Core
import CoreStats as Core

import ErrUtils as Err
import DynFlags
import Outputable
import HscTypes
import Module

import Control.Monad

data Report
   = ReportCore CoreReport


-- | Report on a Core program
data CoreReport = CoreReport
   { coreReportModule  :: Module           -- ^ Module
   , coreReportPass    :: CoreToDo         -- ^ Pass
   , coreReportProgram :: CoreProgram      -- ^ Core program (TODO:input/output)
   , coreReportStats   :: CoreStats        -- ^ Core statistics
   , coreReportCounts  :: Maybe SimplCount -- ^ Pass statistics
   , coreReportRules   :: [CoreRule]       -- ^ Rules
   -- TODO: transformation details (with src spans)
   }

endCorePass :: Module -> HscEnv -> PrintUnqualified
          -> CoreToDo -> Maybe SimplCount -> CoreProgram -> [CoreRule] -> IO ()
endCorePass this_mod hsc_env unqual pass mcounts binds rules = do
  -- build and publish a report
  let report = ReportCore $ CoreReport
         { coreReportModule  = this_mod
         , coreReportPass    = pass
         , coreReportProgram = binds
         , coreReportStats   = coreBindsStats binds
         , coreReportCounts  = mcounts
         , coreReportRules   = rules
         }
 -- publishReport dflags report

  -- default dump behavior
  forM_ mb_flag $ \flag -> do
    Err.dumpSDoc dflags unqual flag
        ("Core - " ++ (showSDoc dflags hdr)) dump_doc

    forM_ mcounts $ \counts ->
      Err.dumpSDoc dflags unqual flag
        ("Statistics - " ++ (showSDoc dflags hdr)) (pprSimplCount counts)

    -- Report result size
    -- This has the side effect of forcing the intermediate to be evaluated
    -- if it's not already forced by a -ddump flag.
    when (verbosity dflags >= 2) -- TODO: replace explicit verbosity check
      $ logInfo dflags (defaultUserStyle dflags) size_doc
  where
    hdr      = ppr pass
    size_doc = text "Core size after" <+> hdr <+> equals
               <+> ppr (coreBindsStats binds)

    dump_doc  = vcat [ text "-- Global size:" <+> ppr (coreBindsStats binds)
                     , blankLine
                     , pprCoreBindingsWithSize binds
                     , ppUnless (null rules) pp_rules ]
    pp_rules = vcat [ blankLine
                    , text "------ Local rules for imported ids --------"
                    , pprRules rules ]
    dflags  = hsc_dflags hsc_env
    mb_flag = case coreDumpFlag pass of
                Just flag | dopt flag dflags                    -> Just flag
                          | dopt Opt_D_verbose_core2core dflags -> Just flag
                _ -> Nothing

coreDumpFlag :: CoreToDo -> Maybe DumpFlag
coreDumpFlag (CoreDoSimplify {})      = Just Opt_D_dump_simpl_iterations
coreDumpFlag (CoreDoPluginPass {})    = Just Opt_D_verbose_core2core
coreDumpFlag CoreDoFloatInwards       = Just Opt_D_verbose_core2core
coreDumpFlag (CoreDoFloatOutwards {}) = Just Opt_D_verbose_core2core
coreDumpFlag CoreLiberateCase         = Just Opt_D_verbose_core2core
coreDumpFlag CoreDoStaticArgs         = Just Opt_D_verbose_core2core
coreDumpFlag CoreDoCallArity          = Just Opt_D_dump_call_arity
coreDumpFlag CoreDoStrictness         = Just Opt_D_dump_stranal
coreDumpFlag CoreDoWorkerWrapper      = Just Opt_D_dump_worker_wrapper
coreDumpFlag CoreDoSpecialising       = Just Opt_D_dump_spec
coreDumpFlag CoreDoSpecConstr         = Just Opt_D_dump_spec
coreDumpFlag CoreCSE                  = Just Opt_D_dump_cse
coreDumpFlag CoreDoVectorisation      = Just Opt_D_dump_vect
coreDumpFlag CoreDesugar              = Just Opt_D_dump_ds
coreDumpFlag CoreDesugarOpt           = Just Opt_D_dump_ds
coreDumpFlag CoreTidy                 = Just Opt_D_dump_simpl
coreDumpFlag CorePrep                 = Just Opt_D_dump_prep

coreDumpFlag CoreDoPrintCore          = Nothing
coreDumpFlag (CoreDoRuleCheck {})     = Nothing
coreDumpFlag CoreDoNothing            = Nothing
coreDumpFlag (CoreDoPasses {})        = Nothing

