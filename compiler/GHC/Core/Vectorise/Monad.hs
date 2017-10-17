module GHC.Core.Vectorise.Monad (
  module GHC.Core.Vectorise.Monad.Base,
  module GHC.Core.Vectorise.Monad.Naming,
  module GHC.Core.Vectorise.Monad.Local,
  module GHC.Core.Vectorise.Monad.Global,
  module GHC.Core.Vectorise.Monad.InstEnv,
  initV,

  -- * Builtins
  liftBuiltinDs,
  builtin,
  builtins,

  -- * Variables
  lookupVar,
  lookupVar_maybe,
  addGlobalParallelVar,
  addGlobalParallelTyCon,
) where

import GHC.Prelude

import GHC.Core.Vectorise.Monad.Base
import GHC.Core.Vectorise.Monad.Naming
import GHC.Core.Vectorise.Monad.Local
import GHC.Core.Vectorise.Monad.Global
import GHC.Core.Vectorise.Monad.InstEnv
import GHC.Core.Vectorise.Builtins
import GHC.Core.Vectorise.Env

import GHC.Core.Syntax
import GHC.Haskell.TypeCheck.Monad
import GHC.HaskellToCore.Monad
import GHC.CoreTypes.Base hiding ( MonadThings(..) )
import GHC.Config.Flags
import GHC.CoreTypes.Instance
import GHC.CoreTypes.Class
import GHC.CoreTypes.TyCon
import GHC.CoreTypes.Name.Set
import GHC.CoreTypes.Var.Set
import GHC.CoreTypes.Var.Environment
import GHC.CoreTypes.Var
import GHC.CoreTypes.Id
import GHC.CoreTypes.Name
import GHC.Util.Error
import GHC.Util.Outputable
import GHC.CoreTypes.Module

import Control.Monad (join)

-- |Run a vectorisation computation.
--
initV :: HscEnv
      -> ModGuts
      -> VectInfo
      -> VM a
      -> IO (Maybe (VectInfo, a))
initV hsc_env guts info thing_inside
  = do { dumpIfVtTrace "Incoming VectInfo" (ppr info)

       ; (_, res) <- initDsWithModGuts hsc_env guts go
       ; case join res of
           Nothing
             -> dumpIfVtTrace "Vectorisation FAILED!" empty
           Just (info', _)
             -> dumpIfVtTrace "Outgoing VectInfo" (ppr info')

       ; return $ join res
       }
  where
    dflags = hsc_dflags hsc_env

    dumpIfVtTrace = dumpIfSet_dyn dflags Opt_D_dump_vt_trace

    bindsToIds (NonRec v _)   = [v]
    bindsToIds (Rec    binds) = map fst binds

    ids = concatMap bindsToIds (mg_binds guts)

    go
      = do {   -- set up tables of builtin entities
           ; builtins        <- initBuiltins
           ; builtin_vars    <- initBuiltinVars builtins

               -- set up class and type family envrionments
           ; eps <- liftIO $ hscEPS hsc_env
           ; let famInstEnvs = (eps_fam_inst_env eps, mg_fam_inst_env guts)
                 instEnvs    = InstEnvs (eps_inst_env     eps)
                                        (mg_inst_env     guts)
                                        (mkModuleSet (dep_orphs (mg_deps guts)))
                 builtin_pas = initClassDicts instEnvs (paClass builtins)  -- grab all 'PA' and..
                 builtin_prs = initClassDicts instEnvs (prClass builtins)  -- ..'PR' class instances

               -- construct the initial global environment
           ; let genv = extendImportedVarsEnv builtin_vars
                        . setPAFunsEnv        builtin_pas
                        . setPRFunsEnv        builtin_prs
                        $ initGlobalEnv (gopt Opt_VectorisationAvoidance dflags)
                                        info (mg_vect_decls guts) instEnvs famInstEnvs

               -- perform vectorisation
           ; r <- runVM thing_inside builtins genv emptyLocalEnv
           ; case r of
               Yes genv _ x -> return $ Just (new_info genv, x)
               No reason    -> do { unqual <- mkPrintUnqualifiedDs
                                  ; liftIO $
                                      printOutputForUser dflags unqual $
                                        mkDumpDoc "Warning: vectorisation failure:" reason
                                  ; return Nothing
                                  }
           }

    new_info genv = modVectInfo genv ids (mg_tcs guts) (mg_vect_decls guts) info

    -- For a given DPH class, produce a mapping from type constructor (in head position) to the
    -- instance dfun for that type constructor and class.  (DPH class instances cannot overlap in
    -- head constructors.)
    --
    initClassDicts :: InstEnvs -> Class -> [(Name, Var)]
    initClassDicts insts cls = map find $ classInstances insts cls
      where
        find i | [Just tc] <- instanceRoughTcs i = (tc, instanceDFunId i)
               | otherwise                       = pprPanic invalidInstance (ppr i)

    invalidInstance = "Invalid DPH instance (overlapping in head constructor)"

-- Builtins -------------------------------------------------------------------

-- |Lift a desugaring computation using the `Builtins` into the vectorisation monad.
--
liftBuiltinDs :: (Builtins -> DsM a) -> VM a
liftBuiltinDs p = VM $ \bi genv lenv -> do { x <- p bi; return (Yes genv lenv x)}

-- |Project something from the set of builtins.
--
builtin :: (Builtins -> a) -> VM a
builtin f = VM $ \bi genv lenv -> return (Yes genv lenv (f bi))

-- |Lift a function using the `Builtins` into the vectorisation monad.
--
builtins :: (a -> Builtins -> b) -> VM (a -> b)
builtins f = VM $ \bi genv lenv -> return (Yes genv lenv (`f` bi))


-- Var ------------------------------------------------------------------------

-- |Lookup the vectorised, and if local, also the lifted version of a variable.
--
-- * If it's in the global environment we get the vectorised version.
-- * If it's in the local environment we get both the vectorised and lifted version.
--
lookupVar :: Var -> VM (Scope Var (Var, Var))
lookupVar v
  = do { mb_res <- lookupVar_maybe v
       ; case mb_res of
           Just x  -> return x
           Nothing ->
               do dflags <- getDynFlags
                  dumpVar dflags v
       }

lookupVar_maybe :: Var -> VM (Maybe (Scope Var (Var, Var)))
lookupVar_maybe v
 = do { r <- readLEnv $ \env -> lookupVarEnv (local_vars env) v
      ; case r of
          Just e  -> return $ Just (Local e)
          Nothing -> fmap Global <$> (readGEnv $ \env -> lookupVarEnv (global_vars env) v)
      }

dumpVar :: DynFlags -> Var -> a
dumpVar dflags var
  | Just _    <- isClassOpId_maybe var
  = cantVectorise dflags "ClassOpId not vectorised:" (ppr var)
  | otherwise
  = cantVectorise dflags "Variable not vectorised:" (ppr var)


-- Global parallel entities ----------------------------------------------------

-- |Mark the given variable as parallel — i.e., executing the associated code might involve
-- parallel array computations.
--
addGlobalParallelVar :: Var -> VM ()
addGlobalParallelVar var
  = do { traceVt "addGlobalParallelVar" (ppr var)
       ; updGEnv $ \env -> env{global_parallel_vars = extendDVarSet (global_parallel_vars env) var}
       }

-- |Mark the given type constructor as parallel — i.e., its values might embed parallel arrays.
--
addGlobalParallelTyCon :: TyCon -> VM ()
addGlobalParallelTyCon tycon
  = do { traceVt "addGlobalParallelTyCon" (ppr tycon)
       ; updGEnv $ \env ->
           env{global_parallel_tycons = extendNameSet (global_parallel_tycons env) (tyConName tycon)}
       }
