module Report
   ( IR (..)
   -- ** Core
   , reportCore
   , endCorePassIO
   , endSimplifierIteration
   )
where

import CoreSyn as Core
import PprCore as Core
import CoreMonad as Core
import CoreStats as Core

import ErrUtils as Err
import DynFlags
import Outputable
import HscTypes
import Module

import Control.Monad

data IR
   = IRCore Core.CoreProgram


reportCore :: DynFlags -> Module -> Core.CoreProgram -> IO ()
reportCore dflags modl core = reportIR dflags modl (IRCore core)

reportIR :: DynFlags -> Module -> IR -> IO ()
reportIR dflags _modl ir = case ir of
   IRCore core -> logDump dflags (pprCoreBindingsWithSize core)


endSimplifierIteration :: Module -> HscEnv -> PrintUnqualified -> CoreToDo
                   -> SimplCount -> CoreProgram -> [CoreRule] -> IO ()
endSimplifierIteration this_mod hsc_env unqual pass counts binds rules
  = endCorePassIO' this_mod hsc_env unqual pass pp_counts binds rules
  where
    pp_counts = Just (pprSimplCount counts)

endCorePassIO :: Module -> HscEnv -> PrintUnqualified
          -> CoreToDo -> CoreProgram -> [CoreRule] -> IO ()
endCorePassIO this_mod hsc_env unqual pass binds rules = do
   endCorePassIO' this_mod hsc_env unqual pass (pprPassStats pass) binds rules


endCorePassIO' :: Module -> HscEnv -> PrintUnqualified
          -> CoreToDo -> Maybe SDoc -> CoreProgram -> [CoreRule] -> IO ()
endCorePassIO' _this_mod hsc_env unqual pass mstats binds rules = do
  forM_ mb_flag $ \flag -> do
    Err.dumpSDoc dflags unqual flag
        ("Core - " ++ (showSDoc dflags hdr)) dump_doc

    forM_ mstats $ \stats ->
      Err.dumpSDoc dflags unqual flag
        ("Statistics - " ++ (showSDoc dflags hdr)) stats

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

pprPassStats :: CoreToDo -> Maybe SDoc
pprPassStats (CoreDoSimplify n md) =
   Just $ vcat [ text "Max iterations:" <+> int n , ppr md ]

pprPassStats _ = Nothing

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

