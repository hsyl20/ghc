{-# LANGUAGE BangPatterns, CPP #-}

-----------------------------------------------------------------------------
--
-- Code generation for ticky-ticky profiling
--
-- (c) The University of Glasgow 2004-2006
--
-----------------------------------------------------------------------------

{- OVERVIEW: ticky ticky profiling

Please see
http://ghc.haskell.org/trac/ghc/wiki/Debugging/TickyTicky and also
edit it and the rest of this comment to keep them up-to-date if you
change ticky-ticky. Thanks!

 *** All allocation ticky numbers are in bytes. ***

Some of the relevant source files:

       ***not necessarily an exhaustive list***

  * some codeGen/ modules import this one

  * this module imports cmm/CLabel.hs to manage labels

  * cmm/CmmParse.y expands some macros using generators defined in
    this module

  * includes/stg/Ticky.h declares all of the global counters

  * includes/rts/Ticky.h declares the C data type for an
    STG-declaration's counters

  * some macros defined in includes/Cmm.h (and used within the RTS's
    CMM code) update the global ticky counters

  * at the end of execution rts/Ticky.c generates the final report
    +RTS -r<report-file> -RTS

The rts/Ticky.c function that generates the report includes an
STG-declaration's ticky counters if

  * that declaration was entered, or

  * it was allocated (if -ticky-allocd)

On either of those events, the counter is "registered" by adding it to
a linked list; cf the CMM generated by registerTickyCtr.

Ticky-ticky profiling has evolved over many years. Many of the
counters from its most sophisticated days are no longer
active/accurate. As the RTS has changed, sometimes the ticky code for
relevant counters was not accordingly updated. Unfortunately, neither
were the comments.

As of March 2013, there still exist deprecated code and comments in
the code generator as well as the RTS because:

  * I don't know what is out-of-date versus merely commented out for
    momentary convenience, and

  * someone else might know how to repair it!

-}

module StgCmmTicky (
  withNewTickyCounterFun,
  withNewTickyCounterLNE,
  withNewTickyCounterThunk,
  withNewTickyCounterStdThunk,
  withNewTickyCounterCon,

  tickyDynAlloc,
  tickyAllocHeap,

  tickyAllocPrim,
  tickyAllocThunk,
  tickyAllocPAP,
  tickyHeapCheck,
  tickyStackCheck,

  tickyUnknownCall, tickyDirectCall,

  tickyPushUpdateFrame,
  tickyUpdateFrameOmitted,

  tickyEnterDynCon,
  tickyEnterStaticCon,
  tickyEnterViaNode,

  tickyEnterFun,
  tickyEnterThunk, tickyEnterStdThunk,        -- dynamic non-value
                                              -- thunks only
  tickyEnterLNE,

  tickyUpdateBhCaf,
  tickyBlackHole,
  tickyUnboxedTupleReturn,
  tickyReturnOldCon, tickyReturnNewCon,

  tickyKnownCallTooFewArgs, tickyKnownCallExact, tickyKnownCallExtraArgs,
  tickySlowCall, tickySlowCallPat,
  ) where

#include "HsVersions.h"

import StgCmmArgRep    ( slowCallPattern , toArgRep , argRepString )
import StgCmmClosure
import StgCmmUtils
import StgCmmMonad

import GHC.STG.Syntax
import GHC.Cmm.Expr
import GHC.Cmm.Graph
import GHC.Cmm.Utils
import GHC.Data.CLabel
import GHC.RTS.Storage

import GHC.Data.Module
import GHC.Data.Name
import GHC.Data.Id
import GHC.Data.BasicTypes
import GHC.Data.FastString
import GHC.Utils.Outputable

import GHC.Config.Flags

-- Turgid imports for showTypeCategory
import PrelNames
import TcType
import GHC.Data.Type
import GHC.Data.Type.Constructor

import Data.Maybe
import qualified Data.Char
import Control.Monad ( unless, when )

-----------------------------------------------------------------------------
--
-- Ticky-ticky profiling
--
-----------------------------------------------------------------------------

data TickyClosureType
    = TickyFun
        Bool -- True <-> single entry
    | TickyCon
    | TickyThunk
        Bool -- True <-> updateable
        Bool -- True <-> standard thunk (AP or selector), has no entry counter
    | TickyLNE

withNewTickyCounterFun :: Bool -> Name  -> [NonVoid Id] -> FCode a -> FCode a
withNewTickyCounterFun single_entry = withNewTickyCounter (TickyFun single_entry)

withNewTickyCounterLNE :: Name  -> [NonVoid Id] -> FCode a -> FCode a
withNewTickyCounterLNE nm args code = do
  b <- tickyLNEIsOn
  if not b then code else withNewTickyCounter TickyLNE nm args code

withNewTickyCounterThunk
  :: Bool -- ^ static
  -> Bool -- ^ updateable
  -> Name
  -> FCode a
  -> FCode a
withNewTickyCounterThunk isStatic isUpdatable name code = do
    b <- tickyDynThunkIsOn
    if isStatic || not b -- ignore static thunks
      then code
      else withNewTickyCounter (TickyThunk isUpdatable False) name [] code

withNewTickyCounterStdThunk
  :: Bool -- ^ updateable
  -> Name
  -> FCode a
  -> FCode a
withNewTickyCounterStdThunk isUpdatable name code = do
    b <- tickyDynThunkIsOn
    if not b
      then code
      else withNewTickyCounter (TickyThunk isUpdatable True) name [] code

withNewTickyCounterCon
  :: Name
  -> FCode a
  -> FCode a
withNewTickyCounterCon name code = do
    b <- tickyDynThunkIsOn
    if not b
      then code
      else withNewTickyCounter TickyCon name [] code

-- args does not include the void arguments
withNewTickyCounter :: TickyClosureType -> Name -> [NonVoid Id] -> FCode a -> FCode a
withNewTickyCounter cloType name args m = do
  lbl <- emitTickyCounter cloType name args
  setTickyCtrLabel lbl m

emitTickyCounter :: TickyClosureType -> Name -> [NonVoid Id] -> FCode CLabel
emitTickyCounter cloType name args
  = let ctr_lbl = mkRednCountsLabel name in
    (>> return ctr_lbl) $
    ifTicky $ do
        { dflags <- getDynFlags
        ; parent <- getTickyCtrLabel
        ; mod_name <- getModuleName

          -- When printing the name of a thing in a ticky file, we
          -- want to give the module name even for *local* things.  We
          -- print just "x (M)" rather that "M.x" to distinguish them
          -- from the global kind.
        ; let ppr_for_ticky_name :: SDoc
              ppr_for_ticky_name =
                let n = ppr name
                    ext = case cloType of
                              TickyFun single_entry -> parens $ hcat $ punctuate comma $
                                  [text "fun"] ++ [text "se"|single_entry]
                              TickyCon -> parens (text "con")
                              TickyThunk upd std -> parens $ hcat $ punctuate comma $
                                  [text "thk"] ++ [text "se"|not upd] ++ [text "std"|std]
                              TickyLNE | isInternalName name -> parens (text "LNE")
                                       | otherwise -> panic "emitTickyCounter: how is this an external LNE?"
                    p = case hasHaskellName parent of
                            -- NB the default "top" ticky ctr does not
                            -- have a Haskell name
                          Just pname -> text "in" <+> ppr (nameUnique pname)
                          _ -> empty
                in if isInternalName name
                   then n <+> parens (ppr mod_name) <+> ext <+> p
                   else n <+> ext <+> p

        ; fun_descr_lit <- newStringCLit $ showSDocDebug dflags ppr_for_ticky_name
        ; arg_descr_lit <- newStringCLit $ map (showTypeCategory . idType . fromNonVoid) args
        ; emitDataLits ctr_lbl
        -- Must match layout of includes/rts/Ticky.h's StgEntCounter
        --
        -- krc: note that all the fields are I32 now; some were I16
        -- before, but the code generator wasn't handling that
        -- properly and it led to chaos, panic and disorder.
            [ mkIntCLit dflags 0,               -- registered?
              mkIntCLit dflags (length args),   -- Arity
              mkIntCLit dflags 0,               -- Heap allocated for this thing
              fun_descr_lit,
              arg_descr_lit,
              zeroCLit dflags,          -- Entries into this thing
              zeroCLit dflags,          -- Heap allocated by this thing
              zeroCLit dflags                   -- Link to next StgEntCounter
            ]
        }

-- -----------------------------------------------------------------------------
-- Ticky stack frames

tickyPushUpdateFrame, tickyUpdateFrameOmitted :: FCode ()
tickyPushUpdateFrame    = ifTicky $ bumpTickyCounter (fsLit "UPDF_PUSHED_ctr")
tickyUpdateFrameOmitted = ifTicky $ bumpTickyCounter (fsLit "UPDF_OMITTED_ctr")

-- -----------------------------------------------------------------------------
-- Ticky entries

-- NB the name-specific entries are only available for names that have
-- dedicated Cmm code. As far as I know, this just rules out
-- constructor thunks. For them, there is no CMM code block to put the
-- bump of name-specific ticky counter into. On the other hand, we can
-- still track allocation their allocation.

tickyEnterDynCon, tickyEnterStaticCon, tickyEnterViaNode :: FCode ()
tickyEnterDynCon      = ifTicky $ bumpTickyCounter (fsLit "ENT_DYN_CON_ctr")
tickyEnterStaticCon   = ifTicky $ bumpTickyCounter (fsLit "ENT_STATIC_CON_ctr")
tickyEnterViaNode     = ifTicky $ bumpTickyCounter (fsLit "ENT_VIA_NODE_ctr")

tickyEnterThunk :: ClosureInfo -> FCode ()
tickyEnterThunk cl_info
  = ifTicky $ do
    { bumpTickyCounter ctr
    ; unless static $ do
      ticky_ctr_lbl <- getTickyCtrLabel
      registerTickyCtrAtEntryDyn ticky_ctr_lbl
      bumpTickyEntryCount ticky_ctr_lbl }
  where
    updatable = closureSingleEntry cl_info
    static    = isStaticClosure cl_info

    ctr | static    = if updatable then fsLit "ENT_STATIC_THK_SINGLE_ctr"
                                   else fsLit "ENT_STATIC_THK_MANY_ctr"
        | otherwise = if updatable then fsLit "ENT_DYN_THK_SINGLE_ctr"
                                   else fsLit "ENT_DYN_THK_MANY_ctr"

tickyEnterStdThunk :: ClosureInfo -> FCode ()
tickyEnterStdThunk = tickyEnterThunk

tickyBlackHole :: Bool{-updatable-} -> FCode ()
tickyBlackHole updatable
  = ifTicky (bumpTickyCounter ctr)
  where
    ctr | updatable = (fsLit "UPD_BH_SINGLE_ENTRY_ctr")
        | otherwise = (fsLit "UPD_BH_UPDATABLE_ctr")

tickyUpdateBhCaf :: ClosureInfo -> FCode ()
tickyUpdateBhCaf cl_info
  = ifTicky (bumpTickyCounter ctr)
  where
    ctr | closureUpdReqd cl_info = (fsLit "UPD_CAF_BH_SINGLE_ENTRY_ctr")
        | otherwise              = (fsLit "UPD_CAF_BH_UPDATABLE_ctr")

tickyEnterFun :: ClosureInfo -> FCode ()
tickyEnterFun cl_info = ifTicky $ do
  ctr_lbl <- getTickyCtrLabel

  if isStaticClosure cl_info
    then do bumpTickyCounter (fsLit "ENT_STATIC_FUN_DIRECT_ctr")
            registerTickyCtr ctr_lbl
    else do bumpTickyCounter (fsLit "ENT_DYN_FUN_DIRECT_ctr")
            registerTickyCtrAtEntryDyn ctr_lbl

  bumpTickyEntryCount ctr_lbl

tickyEnterLNE :: FCode ()
tickyEnterLNE = ifTicky $ do
  bumpTickyCounter (fsLit "ENT_LNE_ctr")
  ifTickyLNE $ do
    ctr_lbl <- getTickyCtrLabel
    registerTickyCtr ctr_lbl
    bumpTickyEntryCount ctr_lbl

-- needn't register a counter upon entry if
--
-- 1) it's for a dynamic closure, and
--
-- 2) -ticky-allocd is on
--
-- since the counter was registered already upon being alloc'd
registerTickyCtrAtEntryDyn :: CLabel -> FCode ()
registerTickyCtrAtEntryDyn ctr_lbl = do
  already_registered <- tickyAllocdIsOn
  when (not already_registered) $ registerTickyCtr ctr_lbl

registerTickyCtr :: CLabel -> FCode ()
-- Register a ticky counter
--   if ( ! f_ct.registeredp ) {
--          f_ct.link = ticky_entry_ctrs;       /* hook this one onto the front of the list */
--          ticky_entry_ctrs = & (f_ct);        /* mark it as "registered" */
--          f_ct.registeredp = 1 }
registerTickyCtr ctr_lbl = do
  dflags <- getDynFlags
  let
    -- krc: code generator doesn't handle Not, so we test for Eq 0 instead
    test = CmmMachOp (MO_Eq (wordWidth dflags))
              [CmmLoad (CmmLit (cmmLabelOffB ctr_lbl
                                (oFFSET_StgEntCounter_registeredp dflags))) (bWord dflags),
               zeroExpr dflags]
    register_stmts
      = [ mkStore (CmmLit (cmmLabelOffB ctr_lbl (oFFSET_StgEntCounter_link dflags)))
                   (CmmLoad ticky_entry_ctrs (bWord dflags))
        , mkStore ticky_entry_ctrs (mkLblExpr ctr_lbl)
        , mkStore (CmmLit (cmmLabelOffB ctr_lbl
                                (oFFSET_StgEntCounter_registeredp dflags)))
                   (mkIntExpr dflags 1) ]
    ticky_entry_ctrs = mkLblExpr (mkCmmDataLabel rtsUnitId (fsLit "ticky_entry_ctrs"))
  emit =<< mkCmmIfThen test (catAGraphs register_stmts)

tickyReturnOldCon, tickyReturnNewCon :: RepArity -> FCode ()
tickyReturnOldCon arity
  = ifTicky $ do { bumpTickyCounter (fsLit "RET_OLD_ctr")
                 ; bumpHistogram    (fsLit "RET_OLD_hst") arity }
tickyReturnNewCon arity
  = ifTicky $ do { bumpTickyCounter (fsLit "RET_NEW_ctr")
                 ; bumpHistogram    (fsLit "RET_NEW_hst") arity }

tickyUnboxedTupleReturn :: RepArity -> FCode ()
tickyUnboxedTupleReturn arity
  = ifTicky $ do { bumpTickyCounter (fsLit "RET_UNBOXED_TUP_ctr")
                 ; bumpHistogram    (fsLit "RET_UNBOXED_TUP_hst") arity }

-- -----------------------------------------------------------------------------
-- Ticky calls

-- Ticks at a *call site*:
tickyDirectCall :: RepArity -> [StgArg] -> FCode ()
tickyDirectCall arity args
  | arity == length args = tickyKnownCallExact
  | otherwise = do tickyKnownCallExtraArgs
                   tickySlowCallPat (map argPrimRep (drop arity args))

tickyKnownCallTooFewArgs :: FCode ()
tickyKnownCallTooFewArgs = ifTicky $ bumpTickyCounter (fsLit "KNOWN_CALL_TOO_FEW_ARGS_ctr")

tickyKnownCallExact :: FCode ()
tickyKnownCallExact      = ifTicky $ bumpTickyCounter (fsLit "KNOWN_CALL_ctr")

tickyKnownCallExtraArgs :: FCode ()
tickyKnownCallExtraArgs  = ifTicky $ bumpTickyCounter (fsLit "KNOWN_CALL_EXTRA_ARGS_ctr")

tickyUnknownCall :: FCode ()
tickyUnknownCall         = ifTicky $ bumpTickyCounter (fsLit "UNKNOWN_CALL_ctr")

-- Tick for the call pattern at slow call site (i.e. in addition to
-- tickyUnknownCall, tickyKnownCallExtraArgs, etc.)
tickySlowCall :: LambdaFormInfo -> [StgArg] -> FCode ()
tickySlowCall _ [] = return ()
tickySlowCall lf_info args = do
 -- see Note [Ticky for slow calls]
 if isKnownFun lf_info
   then tickyKnownCallTooFewArgs
   else tickyUnknownCall
 tickySlowCallPat (map argPrimRep args)

tickySlowCallPat :: [PrimRep] -> FCode ()
tickySlowCallPat args = ifTicky $
  let argReps = map toArgRep args
      (_, n_matched) = slowCallPattern argReps
  in if n_matched > 0 && n_matched == length args
     then bumpTickyLbl $ mkRtsSlowFastTickyCtrLabel $ concatMap (map Data.Char.toLower . argRepString) argReps
     else bumpTickyCounter $ fsLit "VERY_SLOW_CALL_ctr"

{-

Note [Ticky for slow calls]
~~~~~~~~~~~~~~~~~~~~~~~~~~~
Terminology is unfortunately a bit mixed up for these calls. codeGen
uses "slow call" to refer to unknown calls and under-saturated known
calls.

Nowadays, though (ie as of the eval/apply paper), the significantly
slower calls are actually just a subset of these: the ones with no
built-in argument pattern (cf StgCmmArgRep.slowCallPattern)

So for ticky profiling, we split slow calls into
"SLOW_CALL_fast_<pattern>_ctr" (those matching a built-in pattern) and
VERY_SLOW_CALL_ctr (those without a built-in pattern; these are very
bad for both space and time).

-}

-- -----------------------------------------------------------------------------
-- Ticky allocation

tickyDynAlloc :: Maybe Id -> SMRep -> LambdaFormInfo -> FCode ()
-- Called when doing a dynamic heap allocation; the LambdaFormInfo
-- used to distinguish between closure types
--
-- TODO what else to count while we're here?
tickyDynAlloc mb_id rep lf = ifTicky $ getDynFlags >>= \dflags ->
  let bytes = wORD_SIZE dflags * heapClosureSizeW dflags rep

      countGlobal tot ctr = do
        bumpTickyCounterBy tot bytes
        bumpTickyCounter   ctr
      countSpecific = ifTickyAllocd $ case mb_id of
        Nothing -> return ()
        Just id -> do
          let ctr_lbl = mkRednCountsLabel (idName id)
          registerTickyCtr ctr_lbl
          bumpTickyAllocd ctr_lbl bytes

  -- TODO are we still tracking "good stuff" (_gds) versus
  -- administrative (_adm) versus slop (_slp)? I'm going with all _gds
  -- for now, since I don't currently know neither if we do nor how to
  -- distinguish. NSF Mar 2013

  in case () of
    _ | isConRep rep   ->
          ifTickyDynThunk countSpecific >>
          countGlobal (fsLit "ALLOC_CON_gds") (fsLit "ALLOC_CON_ctr")
      | isThunkRep rep ->
          ifTickyDynThunk countSpecific >>
          if lfUpdatable lf
          then countGlobal (fsLit "ALLOC_THK_gds") (fsLit "ALLOC_UP_THK_ctr")
          else countGlobal (fsLit "ALLOC_THK_gds") (fsLit "ALLOC_SE_THK_ctr")
      | isFunRep   rep ->
          countSpecific >>
          countGlobal (fsLit "ALLOC_FUN_gds") (fsLit "ALLOC_FUN_ctr")
      | otherwise      -> panic "How is this heap object not a con, thunk, or fun?"



tickyAllocHeap ::
  Bool -> -- is this a genuine allocation? As opposed to
          -- StgCmmLayout.adjustHpBackwards
  VirtualHpOffset -> FCode ()
-- Called when doing a heap check [TICK_ALLOC_HEAP]
-- Must be lazy in the amount of allocation!
tickyAllocHeap genuine hp
  = ifTicky $
    do  { dflags <- getDynFlags
        ; ticky_ctr <- getTickyCtrLabel
        ; emit $ catAGraphs $
            -- only test hp from within the emit so that the monadic
            -- computation itself is not strict in hp (cf knot in
            -- StgCmmMonad.getHeapUsage)
          if hp == 0 then []
          else let !bytes = wORD_SIZE dflags * hp in [
            -- Bump the allocation total in the closure's StgEntCounter
            addToMem (rEP_StgEntCounter_allocs dflags)
                     (CmmLit (cmmLabelOffB ticky_ctr (oFFSET_StgEntCounter_allocs dflags)))
                     bytes,
            -- Bump the global allocation total ALLOC_HEAP_tot
            addToMemLbl (bWord dflags)
                        (mkCmmDataLabel rtsUnitId (fsLit "ALLOC_HEAP_tot"))
                        bytes,
            -- Bump the global allocation counter ALLOC_HEAP_ctr
            if not genuine then mkNop
            else addToMemLbl (bWord dflags)
                             (mkCmmDataLabel rtsUnitId (fsLit "ALLOC_HEAP_ctr"))
                             1
            ]}


--------------------------------------------------------------------------------
-- these three are only called from CmmParse.y (ie ultimately from the RTS)

-- the units are bytes

tickyAllocPrim :: CmmExpr  -- ^ size of the full header, in bytes
               -> CmmExpr  -- ^ size of the payload, in bytes
               -> CmmExpr -> FCode ()
tickyAllocPrim _hdr _goods _slop = ifTicky $ do
  bumpTickyCounter    (fsLit "ALLOC_PRIM_ctr")
  bumpTickyCounterByE (fsLit "ALLOC_PRIM_adm") _hdr
  bumpTickyCounterByE (fsLit "ALLOC_PRIM_gds") _goods
  bumpTickyCounterByE (fsLit "ALLOC_PRIM_slp") _slop

tickyAllocThunk :: CmmExpr -> CmmExpr -> FCode ()
tickyAllocThunk _goods _slop = ifTicky $ do
    -- TODO is it ever called with a Single-Entry thunk?
  bumpTickyCounter    (fsLit "ALLOC_UP_THK_ctr")
  bumpTickyCounterByE (fsLit "ALLOC_THK_gds") _goods
  bumpTickyCounterByE (fsLit "ALLOC_THK_slp") _slop

tickyAllocPAP :: CmmExpr -> CmmExpr -> FCode ()
tickyAllocPAP _goods _slop = ifTicky $ do
  bumpTickyCounter    (fsLit "ALLOC_PAP_ctr")
  bumpTickyCounterByE (fsLit "ALLOC_PAP_gds") _goods
  bumpTickyCounterByE (fsLit "ALLOC_PAP_slp") _slop

tickyHeapCheck :: FCode ()
tickyHeapCheck = ifTicky $ bumpTickyCounter (fsLit "HEAP_CHK_ctr")

tickyStackCheck :: FCode ()
tickyStackCheck = ifTicky $ bumpTickyCounter (fsLit "STK_CHK_ctr")

-- -----------------------------------------------------------------------------
-- Ticky utils

ifTicky :: FCode () -> FCode ()
ifTicky code =
  getDynFlags >>= \dflags -> when (gopt Opt_Ticky dflags) code

tickyAllocdIsOn :: FCode Bool
tickyAllocdIsOn = gopt Opt_Ticky_Allocd `fmap` getDynFlags

tickyLNEIsOn :: FCode Bool
tickyLNEIsOn = gopt Opt_Ticky_LNE `fmap` getDynFlags

tickyDynThunkIsOn :: FCode Bool
tickyDynThunkIsOn = gopt Opt_Ticky_Dyn_Thunk `fmap` getDynFlags

ifTickyAllocd :: FCode () -> FCode ()
ifTickyAllocd code = tickyAllocdIsOn >>= \b -> when b code

ifTickyLNE :: FCode () -> FCode ()
ifTickyLNE code = tickyLNEIsOn >>= \b -> when b code

ifTickyDynThunk :: FCode () -> FCode ()
ifTickyDynThunk code = tickyDynThunkIsOn >>= \b -> when b code

bumpTickyCounter :: FastString -> FCode ()
bumpTickyCounter lbl = bumpTickyLbl (mkCmmDataLabel rtsUnitId lbl)

bumpTickyCounterBy :: FastString -> Int -> FCode ()
bumpTickyCounterBy lbl = bumpTickyLblBy (mkCmmDataLabel rtsUnitId lbl)

bumpTickyCounterByE :: FastString -> CmmExpr -> FCode ()
bumpTickyCounterByE lbl = bumpTickyLblByE (mkCmmDataLabel rtsUnitId lbl)

bumpTickyEntryCount :: CLabel -> FCode ()
bumpTickyEntryCount lbl = do
  dflags <- getDynFlags
  bumpTickyLit (cmmLabelOffB lbl (oFFSET_StgEntCounter_entry_count dflags))

bumpTickyAllocd :: CLabel -> Int -> FCode ()
bumpTickyAllocd lbl bytes = do
  dflags <- getDynFlags
  bumpTickyLitBy (cmmLabelOffB lbl (oFFSET_StgEntCounter_allocd dflags)) bytes

bumpTickyLbl :: CLabel -> FCode ()
bumpTickyLbl lhs = bumpTickyLitBy (cmmLabelOffB lhs 0) 1

bumpTickyLblBy :: CLabel -> Int -> FCode ()
bumpTickyLblBy lhs = bumpTickyLitBy (cmmLabelOffB lhs 0)

bumpTickyLblByE :: CLabel -> CmmExpr -> FCode ()
bumpTickyLblByE lhs = bumpTickyLitByE (cmmLabelOffB lhs 0)

bumpTickyLit :: CmmLit -> FCode ()
bumpTickyLit lhs = bumpTickyLitBy lhs 1

bumpTickyLitBy :: CmmLit -> Int -> FCode ()
bumpTickyLitBy lhs n = do
  dflags <- getDynFlags
  emit (addToMem (bWord dflags) (CmmLit lhs) n)

bumpTickyLitByE :: CmmLit -> CmmExpr -> FCode ()
bumpTickyLitByE lhs e = do
  dflags <- getDynFlags
  emit (addToMemE (bWord dflags) (CmmLit lhs) e)

bumpHistogram :: FastString -> Int -> FCode ()
bumpHistogram lbl n = do
    dflags <- getDynFlags
    let offset = n `min` (tICKY_BIN_COUNT dflags - 1)
    emit (addToMem (bWord dflags)
           (cmmIndexExpr dflags
                (wordWidth dflags)
                (CmmLit (CmmLabel (mkCmmDataLabel rtsUnitId lbl)))
                (CmmLit (CmmInt (fromIntegral offset) (wordWidth dflags))))
           1)

------------------------------------------------------------------
-- Showing the "type category" for ticky-ticky profiling

showTypeCategory :: Type -> Char
  {-
        +           dictionary

        >           function

        {C,I,F,D,W} char, int, float, double, word
        {c,i,f,d,w} unboxed ditto

        T           tuple

        P           other primitive type
        p           unboxed ditto

        L           list
        E           enumeration type
        S           other single-constructor type
        M           other multi-constructor data-con type

        .           other type

        -           reserved for others to mark as "uninteresting"

  Accurate as of Mar 2013, but I eliminated the Array category instead
  of updating it, for simplicity. It's in P/p, I think --NSF

    -}
showTypeCategory ty
  | isDictTy ty = '+'
  | otherwise = case tcSplitTyConApp_maybe ty of
  Nothing -> '.'
  Just (tycon, _) ->
    (if isUnliftedTyCon tycon then Data.Char.toLower else id) $
    let anyOf us = getUnique tycon `elem` us in
    case () of
      _ | anyOf [funTyConKey] -> '>'
        | anyOf [charPrimTyConKey, charTyConKey] -> 'C'
        | anyOf [doublePrimTyConKey, doubleTyConKey] -> 'D'
        | anyOf [floatPrimTyConKey, floatTyConKey] -> 'F'
        | anyOf [intPrimTyConKey, int32PrimTyConKey, int64PrimTyConKey,
                 intTyConKey, int8TyConKey, int16TyConKey, int32TyConKey, int64TyConKey
                ] -> 'I'
        | anyOf [wordPrimTyConKey, word32PrimTyConKey, word64PrimTyConKey, wordTyConKey,
                 word8TyConKey, word16TyConKey, word32TyConKey, word64TyConKey
                ] -> 'W'
        | anyOf [listTyConKey] -> 'L'
        | isTupleTyCon tycon       -> 'T'
        | isPrimTyCon tycon        -> 'P'
        | isEnumerationTyCon tycon -> 'E'
        | isJust (tyConSingleDataCon_maybe tycon) -> 'S'
        | otherwise -> 'M' -- oh, well...
