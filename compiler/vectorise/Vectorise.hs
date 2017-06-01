-- Main entry point to the vectoriser.  It is invoked iff the option '-fvectorise' is passed.
--
-- This module provides the function 'vectorise', which vectorises an entire (desugared) module.
-- It vectorises all type declarations and value bindings.  It also processes all VECTORISE pragmas
-- (aka vectorisation declarations), which can lead to the vectorisation of imported data types
-- and the enrichment of imported functions with vectorised versions.

module Vectorise ( vectorise )
where

import Vectorise.Type.Env
import Vectorise.Type.Type
import Vectorise.Convert
import Vectorise.Utils.Hoisting
import Vectorise.Exp
import Vectorise.Env
import Vectorise.Monad

import GHC.Types hiding      ( MonadThings(..) )
import GHC.Core.Optimise.Unfolding           ( mkInlineUnfoldingWithArity )
import GHC.Core.PrettyPrint
import GHC.Core.Syntax
import GHC.Core.Monad            ( CoreM, getHscEnv )
import GHC.Data.Type
import GHC.Data.Id
import GHC.Config.Flags
import GHC.Utils.Outputable
import GHC.Utils                 ( zipLazy )
import GHC.Utils.Monad

import Control.Monad


-- |Vectorise a single module.
--
vectorise :: ModGuts -> CoreM ModGuts
vectorise guts
 = do { hsc_env <- getHscEnv
      ; liftIO $ vectoriseIO hsc_env guts
      }

-- Vectorise a single monad, given the dynamic compiler flags and HscEnv.
--
vectoriseIO :: HscEnv -> ModGuts -> IO ModGuts
vectoriseIO hsc_env guts
 = do {   -- Get information about currently loaded external packages.
      ; eps <- hscEPS hsc_env

          -- Combine vectorisation info from the current module, and external ones.
      ; let info = hptVectInfo hsc_env `plusVectInfo` eps_vect_info eps

          -- Run the main VM computation.
      ; Just (info', guts') <- initV hsc_env guts info (vectModule guts)
      ; return (guts' { mg_vect_info = info' })
      }

-- Vectorise a single module, in the VM monad.
--
vectModule :: ModGuts -> VM ModGuts
vectModule guts@(ModGuts { mg_tcs        = tycons
                         , mg_binds      = binds
                         , mg_fam_insts  = fam_insts
                         , mg_vect_decls = vect_decls
                         })
 = do { dumpOptVt Opt_D_dump_vt_trace "Before vectorisation" $
          pprCoreBindings binds

          -- Pick out all 'VECTORISE [SCALAR] type' and 'VECTORISE class' pragmas
      ; let ty_vect_decls  = [vd | vd@(VectType _ _ _) <- vect_decls]
            cls_vect_decls = [vd | vd@(VectClass _)    <- vect_decls]

          -- Vectorise the type environment.  This will add vectorised
          -- type constructors, their representations, and the
          -- corresponding data constructors.  Moreover, we produce
          -- bindings for dfuns and family instances of the classes
          -- and type families used in the DPH library to represent
          -- array types.
      ; (new_tycons, new_fam_insts, tc_binds) <- vectTypeEnv tycons ty_vect_decls cls_vect_decls

          -- Family instance environment for /all/ home-package modules including those instances
          -- generated by 'vectTypeEnv'.
      ; (_, fam_inst_env) <- readGEnv global_fam_inst_env

          -- Vectorise all the top level bindings and VECTORISE declarations on imported identifiers
          -- NB: Need to vectorise the imported bindings first (local bindings may depend on them).
      ; let impBinds = [(imp_id, expr) | Vect imp_id expr <- vect_decls, isGlobalId imp_id]
      ; binds_imp <- mapM vectImpBind impBinds
      ; binds_top <- mapM vectTopBind binds

      ; return $ guts { mg_tcs          = tycons ++ new_tycons
                        -- we produce no new classes or instances, only new class type constructors
                        -- and dfuns
                      , mg_binds        = Rec tc_binds : (binds_top ++ binds_imp)
                      , mg_fam_inst_env = fam_inst_env
                      , mg_fam_insts    = fam_insts ++ new_fam_insts
                      }
      }

-- Try to vectorise a top-level binding.  If it doesn't vectorise, or if it is entirely scalar, then
-- omit vectorisation of that binding.
--
-- For example, for the binding
--
-- @
--    foo :: Int -> Int
--    foo = \x -> x + x
-- @
--
-- we get
-- @
--    foo  :: Int -> Int
--    foo  = \x -> vfoo $: x
--
--    v_foo :: Closure void vfoo lfoo
--    v_foo = closure vfoo lfoo void
--
--    vfoo :: Void -> Int -> Int
--    vfoo = ...
--
--    lfoo :: PData Void -> PData Int -> PData Int
--    lfoo = ...
-- @
--
-- @vfoo@ is the "vectorised", or scalar, version that does the same as the original function foo,
-- but takes an explicit environment.
--
-- @lfoo@ is the "lifted" version that works on arrays.
--
-- @v_foo@ combines both of these into a `Closure` that also contains the environment.
--
-- The original binding @foo@ is rewritten to call the vectorised version present in the closure.
--
-- Vectorisation may be suppressed by annotating a binding with a 'NOVECTORISE' pragma.  If this
-- pragma is used in a group of mutually recursive bindings, either all or no binding must have
-- the pragma.  If only some bindings are annotated, a fatal error is being raised. (In the case of
-- scalar bindings, we only omit vectorisation if all bindings in a group are scalar.)
--
-- FIXME: Once we support partial vectorisation, we may be able to vectorise parts of a group, or
--   we may emit a warning and refrain from vectorising the entire group.
--
vectTopBind :: CoreBind -> VM CoreBind
vectTopBind b@(NonRec var expr)
  = do
    { traceVt "= Vectorise non-recursive top-level variable" (ppr var)

    ; (hasNoVect, vectDecl) <- lookupVectDecl var
    ; if hasNoVect
      then do
      {   -- 'NOVECTORISE' pragma => leave this binding as it is
      ; traceVt "NOVECTORISE" $ ppr var
      ; return b
      }
      else do
    { vectRhs <- case vectDecl of
        Just (_, expr') ->
            -- 'VECTORISE' pragma => just use the provided vectorised rhs
          do
          { traceVt "VECTORISE" $ ppr var
          ; addGlobalParallelVar var
          ; return $ Just (False, inlineMe, expr')
          }
        Nothing         ->
            -- no pragma => standard vectorisation of rhs
          do
          { traceVt "[Vanilla]" $ ppr var <+> char '=' <+> ppr expr
          ; vectTopExpr var expr
          }
    ; hs <- takeHoisted -- make sure we clean those out (even if we skip)
    ; case vectRhs of
      { Nothing ->
          -- scalar binding => leave this binding as it is
          do
          { traceVt "scalar binding [skip]" $ ppr var
          ; return b
          }
      ; Just (parBind, inline, expr') -> do
    {
       -- vanilla case => create an appropriate top-level binding & add it to the vectorisation map
    ; when parBind $
        addGlobalParallelVar var
    ; var' <- vectTopBinder var inline expr'

        -- We replace the original top-level binding by a value projected from the vectorised
        -- closure and add any newly created hoisted top-level bindings.
    ; cexpr <- tryConvert var var' expr
    ; return . Rec $ (var, cexpr) : (var', expr') : hs
    } } } }
    `orElseErrV`
    do
    { emitVt "  Could NOT vectorise top-level binding" $ ppr var
    ; return b
    }
vectTopBind b@(Rec binds)
  = do
    { traceVt "= Vectorise recursive top-level variables" $ ppr vars

    ; vectDecls <- mapM lookupVectDecl vars
    ; let hasNoVects = map fst vectDecls
    ; if and hasNoVects
      then do
      {   -- 'NOVECTORISE' pragmas => leave this entire binding group as it is
      ; traceVt "NOVECTORISE" $ ppr vars
      ; return b
      }
      else do
    { if or hasNoVects
      then do
        {   -- Inconsistent 'NOVECTORISE' pragmas => bail out
        ; dflags <- getDynFlags
        ; cantVectorise dflags noVectoriseErr (ppr b)
        }
      else do
    { traceVt "[Vanilla]" $ vcat [ppr var <+> char '=' <+> ppr expr | (var, expr) <- binds]

       -- For all bindings *with* a pragma, just use the pragma-supplied vectorised expression
    ; newBindsWPragma  <- concat <$>
                          sequence [ vectTopBindAndConvert bind inlineMe expr'
                                   | (bind, (_, Just (_, expr'))) <- zip binds vectDecls]

        -- Standard vectorisation of all rhses that are *without* a pragma.
        -- NB: The reason for 'fixV' is rather subtle: 'vectTopBindAndConvert' adds entries for
        --     the bound variables in the recursive group to the vectorisation map, which in turn
        --     are needed by 'vectPolyExprs' (unless it returns 'Nothing').
    ; let bindsWOPragma = [bind | (bind, (_, Nothing)) <- zip binds vectDecls]
    ; (newBinds, _) <- fixV $
        \ ~(_, exprs') ->
          do
          {   -- Create appropriate top-level bindings, enter them into the vectorisation map, and
              -- vectorise the right-hand sides
          ; newBindsWOPragma <- concat <$>
                                sequence [vectTopBindAndConvert bind inline expr
                                         | (bind, ~(inline, expr)) <- zipLazy bindsWOPragma exprs']
                                         -- irrefutable pattern and 'zipLazy' to tie the knot;
                                         -- hence, can't use 'zipWithM'
          ; vectRhses <- vectTopExprs bindsWOPragma
          ; hs <- takeHoisted -- make sure we clean those out (even if we skip)

          ; case vectRhses of
              Nothing ->
                -- scalar bindings => skip all bindings except those with pragmas and retract the
                --   entries into the vectorisation map for the scalar bindings
                do
                { traceVt "scalar bindings [skip]" $ ppr vars
                ; mapM_ (undefGlobalVar . fst) bindsWOPragma
                ; return (bindsWOPragma ++ newBindsWPragma, exprs')
                }
              Just (parBind, exprs') ->
                -- vanilla case => record parallel variables and return the final bindings
                do
                { when parBind $
                    mapM_ addGlobalParallelVar vars
                ; return (newBindsWOPragma ++ newBindsWPragma ++ hs, exprs')
                }
          }
    ; return $ Rec newBinds
    } } }
    `orElseErrV`
    do
    { emitVt "  Could NOT vectorise top-level bindings" $ ppr vars
    ; return b
    }
  where
    vars = map fst binds
    noVectoriseErr = "NOVECTORISE must be used on all or no bindings of a recursive group"

    -- Replace the original top-level bindings by a values projected from the vectorised
    -- closures and add any newly created hoisted top-level bindings to the group.
    vectTopBindAndConvert (var, expr) inline expr'
      = do
        { var'  <- vectTopBinder var inline expr'
        ; cexpr <- tryConvert var var' expr
        ; return [(var, cexpr), (var', expr')]
        }

-- Add a vectorised binding to an imported top-level variable that has a VECTORISE pragma
-- in this module.
--
-- RESTRICTION: Currently, we cannot use the pragma for mutually recursive definitions.
--
vectImpBind :: (Id, CoreExpr) -> VM CoreBind
vectImpBind (var, expr)
  = do
    { traceVt "= Add vectorised binding to imported variable" (ppr var)

    ; var' <- vectTopBinder var inlineMe expr
    ; return $ NonRec var' expr
    }

-- |Make the vectorised version of this top level binder, and add the mapping between it and the
-- original to the state. For some binder @foo@ the vectorised version is @$v_foo@
--
-- NOTE: 'vectTopBinder' *MUST* be lazy in inline and expr because of how it is used inside of
--       'fixV' in 'vectTopBind'.
--
vectTopBinder :: Var      -- ^ Name of the binding.
              -> Inline   -- ^ Whether it should be inlined, used to annotate it.
              -> CoreExpr -- ^ RHS of binding, used to set the 'Unfolding' of the returned 'Var'.
              -> VM Var   -- ^ Name of the vectorised binding.
vectTopBinder var inline expr
 = do {   -- Vectorise the type attached to the var.
      ; vty  <- vectType (idType var)

          -- If there is a vectorisation declaration for this binding, make sure its type matches
      ; (_, vectDecl) <- lookupVectDecl var
      ; case vectDecl of
          Nothing             -> return ()
          Just (vdty, _)
            | eqType vty vdty -> return ()
            | otherwise       ->
              do
              { dflags <- getDynFlags
              ; cantVectorise dflags ("Type mismatch in vectorisation pragma for " ++ showPpr dflags var) $
                  (text "Expected type" <+> ppr vty)
                  $$
                  (text "Inferred type" <+> ppr vdty)
              }
          -- Make the vectorised version of binding's name, and set the unfolding used for inlining
      ; var' <- liftM (`setIdUnfolding` unfolding)
                $  mkVectId var vty

          -- Add the mapping between the plain and vectorised name to the state.
      ; defGlobalVar var var'

      ; return var'
    }
  where
    unfolding = case inline of
                  Inline arity -> mkInlineUnfoldingWithArity arity expr
                  DontInline   -> noUnfolding
{-
!!!TODO: dfuns and unfoldings:
           -- Do not inline the dfun; instead give it a magic DFunFunfolding
           -- See Note [ClassOp/DFun selection]
           -- See also note [Single-method classes]
        dfun_id_w_fun
           | isNewTyCon class_tc
           = dfun_id `setInlinePragma` alwaysInlinePragma { inl_sat = Just 0 }
           | otherwise
           = dfun_id `setIdUnfolding`  mkDFunUnfolding dfun_ty dfun_args
                     `setInlinePragma` dfunInlinePragma
 -}

-- |Project out the vectorised version of a binding from some closure, or return the original body
-- if that doesn't work.
--
tryConvert :: Var       -- ^Name of the original binding (eg @foo@)
           -> Var       -- ^Name of vectorised version of binding (eg @$vfoo@)
           -> CoreExpr  -- ^The original body of the binding.
           -> VM CoreExpr
tryConvert var vect_var rhs
  = fromVect (idType var) (Var vect_var)
    `orElseErrV`
    do
    { emitVt "  Could NOT call vectorised from original version" $ ppr var <+> dcolon <+> ppr (idType var)
    ; return rhs
    }
