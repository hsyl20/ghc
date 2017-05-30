{-# LANGUAGE TypeFamilies #-}

{-|
Note [CSE for Stg]
~~~~~~~~~~~~~~~~~~
This module implements a simple common subexpression elimination pass for STG.
This is useful because there are expressions that we want to common up (because
they are operational equivalent), but that we cannot common up in Core, because
their types differ.
This was original reported as #9291.

There are two types of common code occurrences that we aim for, see
note [Case 1: CSEing allocated closures] and
note [Case 2: CSEing case binders] below.


Note [Case 1: CSEing allocated closures]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The fist kind of CSE opportunity we aim for is generated by this Haskell code:

    bar :: a -> (Either Int a, Either Bool a)
    bar x = (Right x, Right x)

which produces this Core:

    bar :: forall a. a -> (Either Int a, Either Bool a)
    bar @a x = (Right @Int @a x, Right @Bool @a x)

where the two components of the tuple are different terms, and cannot be
commoned up (easily). On the STG level we have

    bar [x] = let c1 = Right [x]
                  c2 = Right [x]
              in (c1,c2)

and now it is obvious that we can write

    bar [x] = let c1 = Right [x]
              in (c1,c1)

instead.


Note [Case 2: CSEing case binders]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The second kind of CSE opportunity we aim for is more interesting, and
came up in #9291 and #5344: The Haskell code

    foo :: Either Int a -> Either Bool a
    foo (Right x) = Right x
    foo _         = Left False

produces this Core

    foo :: forall a. Either Int a -> Either Bool a
    foo @a e = case e of b { Left n -> …
                           , Right x -> Right @Bool @a x }

where we cannot CSE `Right @Bool @a x` with the case binder `b` as they have
different types. But in STG we have

    foo [e] = case e of b { Left [n] -> …
                          , Right [x] -> Right [x] }

and nothing stops us from transforming that to

    foo [e] = case e of b { Left [n] -> …
                          , Right [x] -> b}

-}
module StgCse (stgCse) where

import DataCon
import Id
import StgSyn
import GHC.Utils.Outputable
import VarEnv
import CoreSyn (AltCon(..))
import Data.List (mapAccumL)
import Data.Maybe (fromMaybe)
import TrieMap
import NameEnv
import Control.Monad( (>=>) )

--------------
-- The Trie --
--------------

-- A lookup trie for data constructor applications, i.e.
-- keys of type `(DataCon, [StgArg])`, following the patterns in TrieMap.

data StgArgMap a = SAM
    { sam_var :: DVarEnv a
    , sam_lit :: LiteralMap a
    }

instance TrieMap StgArgMap where
    type Key StgArgMap = StgArg
    emptyTM  = SAM { sam_var = emptyTM
                   , sam_lit = emptyTM }
    lookupTM (StgVarArg var) = sam_var >.> lkDFreeVar var
    lookupTM (StgLitArg lit) = sam_lit >.> lookupTM lit
    alterTM  (StgVarArg var) f m = m { sam_var = sam_var m |> xtDFreeVar var f }
    alterTM  (StgLitArg lit) f m = m { sam_lit = sam_lit m |> alterTM lit f }
    foldTM k m = foldTM k (sam_var m) . foldTM k (sam_lit m)
    mapTM f (SAM {sam_var = varm, sam_lit = litm}) =
        SAM { sam_var = mapTM f varm, sam_lit = mapTM f litm }

newtype ConAppMap a = CAM { un_cam :: DNameEnv (ListMap StgArgMap a) }

instance TrieMap ConAppMap where
    type Key ConAppMap = (DataCon, [StgArg])
    emptyTM  = CAM emptyTM
    lookupTM (dataCon, args) = un_cam >.> lkDNamed dataCon >=> lookupTM args
    alterTM  (dataCon, args) f m =
        m { un_cam = un_cam m |> xtDNamed dataCon |>> alterTM args f }
    foldTM k = un_cam >.> foldTM (foldTM k)
    mapTM f  = un_cam >.> mapTM (mapTM f) >.> CAM

-----------------
-- The CSE Env --
-----------------

-- | The CSE environment. See note [CseEnv Example]
data CseEnv = CseEnv
    { ce_conAppMap :: ConAppMap OutId
        -- ^ The main component of the environment is the trie that maps
        --   data constructor applications (with their `OutId` arguments)
        --   to an in-scope name that can be used instead.
        --   This name is always either a let-bound variable or a case binder.
    , ce_subst     :: IdEnv OutId
        -- ^ This substitution is applied to the code as we traverse it.
        --   Entries have one of two reasons:
        --
        --   * The input might have shadowing (see Note [Shadowing]), so we have
        --     to rename some binders as we traverse the tree.
        --   * If we remove `let x = Con z` because  `let y = Con z` is in scope,
        --     we note this here as x ↦ y.
    , ce_bndrMap     :: IdEnv OutId
        --   If we come across a case expression case x as b of … with a trivial
        --   binder, we add b ↦ x to this.
        --   This map is *only* used when looking something up in the ce_conAppMap.
        --   See Note [Trivial case scrutinee]
    , ce_in_scope  :: InScopeSet
        -- ^ The third component is an in-scope set, to rename away any
        --   shadowing binders
    }

{-|
Note [CseEnv Example]
~~~~~~~~~~~~~~~~~~~~~
The following tables shows how the CseEnvironment changes as code is traversed,
as well as the changes to that code.

  InExpr                         OutExpr
     conAppMap                   subst          in_scope
  ───────────────────────────────────────────────────────────
  -- empty                       {}             {}
  case … as a of {Con x y ->     case … as a of {Con x y ->
  -- Con x y ↦ a                 {}             {a,x,y}
  let b = Con x y                (removed)
  -- Con x y ↦ a                 b↦a            {a,x,y,b}
  let c = Bar a                  let c = Bar a
  -- Con x y ↦ a, Bar a ↦ c      b↦a            {a,x,y,b,c}
  let c = some expression        let c' = some expression
  -- Con x y ↦ a, Bar a ↦ c      b↦a, c↦c',     {a,x,y,b,c,c'}
  let d = Bar b                  (removed)
  -- Con x y ↦ a, Bar a ↦ c      b↦a, c↦c', d↦c {a,x,y,b,c,c',d}
  (a, b, c d)                    (a, a, c' c)
-}

initEnv :: InScopeSet -> CseEnv
initEnv in_scope = CseEnv
    { ce_conAppMap = emptyTM
    , ce_subst     = emptyVarEnv
    , ce_bndrMap   = emptyVarEnv
    , ce_in_scope  = in_scope
    }

envLookup :: DataCon -> [OutStgArg] -> CseEnv -> Maybe OutId
envLookup dataCon args env = lookupTM (dataCon, args') (ce_conAppMap env)
  where args' = map go args -- See Note [Trivial case scrutinee]
        go (StgVarArg v  ) = StgVarArg (fromMaybe v $ lookupVarEnv (ce_bndrMap env) v)
        go (StgLitArg lit) = StgLitArg lit

addDataCon :: OutId -> DataCon -> [OutStgArg] -> CseEnv -> CseEnv
-- do not bother with nullary data constructors, they are static anyways
addDataCon _ _ [] env = env
addDataCon bndr dataCon args env = env { ce_conAppMap = new_env }
  where
    new_env = insertTM (dataCon, args) bndr (ce_conAppMap env)

forgetCse :: CseEnv -> CseEnv
forgetCse env = env { ce_conAppMap = emptyTM }
    -- See note [Free variables of an StgClosure]

addSubst :: OutId -> OutId -> CseEnv -> CseEnv
addSubst from to env
    = env { ce_subst = extendVarEnv (ce_subst env) from to }

addTrivCaseBndr :: OutId -> OutId -> CseEnv -> CseEnv
addTrivCaseBndr from to env
    = env { ce_bndrMap = extendVarEnv (ce_bndrMap env) from to }

substArgs :: CseEnv -> [InStgArg] -> [OutStgArg]
substArgs env = map (substArg env)

substArg :: CseEnv -> InStgArg -> OutStgArg
substArg env (StgVarArg from) = StgVarArg (substVar env from)
substArg _   (StgLitArg lit)  = StgLitArg lit

substVars :: CseEnv -> [InId] -> [OutId]
substVars env = map (substVar env)

substVar :: CseEnv -> InId -> OutId
substVar env id = fromMaybe id $ lookupVarEnv (ce_subst env) id

-- Functions to enter binders

-- This is much simpler than the requivalent code in CoreSubst:
--  * We do not substitute type variables, and
--  * There is nothing relevant in IdInfo at this stage
--    that needs substitutions.
-- Therefore, no special treatment for a recursive group is required.

substBndr :: CseEnv -> InId -> (CseEnv, OutId)
substBndr env old_id
  = (new_env, new_id)
  where
    new_id = uniqAway (ce_in_scope env) old_id
    no_change = new_id == old_id
    env' = env { ce_in_scope = ce_in_scope env `extendInScopeSet` new_id }
    new_env | no_change = env' { ce_subst = extendVarEnv (ce_subst env) old_id new_id }
            | otherwise = env'

substBndrs :: CseEnv -> [InVar] -> (CseEnv, [OutVar])
substBndrs env bndrs = mapAccumL substBndr env bndrs

substPairs :: CseEnv -> [(InVar, a)] -> (CseEnv, [(OutVar, a)])
substPairs env bndrs = mapAccumL go env bndrs
  where go env (id, x) = let (env', id') = substBndr env id
                         in (env', (id', x))

-- Main entry point

stgCse :: [InStgTopBinding] -> [OutStgTopBinding]
stgCse binds = snd $ mapAccumL stgCseTopLvl emptyInScopeSet binds

-- Top level bindings.
--
-- We do not CSE these, as top-level closures are allocated statically anyways.
-- Also, they might be exported.
-- But we still have to collect the set of in-scope variables, otherwise
-- uniqAway might shadow a top-level closure.

stgCseTopLvl :: InScopeSet -> InStgTopBinding -> (InScopeSet, OutStgTopBinding)
stgCseTopLvl in_scope t@(StgTopStringLit _ _) = (in_scope, t)
stgCseTopLvl in_scope (StgTopLifted (StgNonRec bndr rhs))
    = (in_scope'
      , StgTopLifted (StgNonRec bndr (stgCseTopLvlRhs in_scope rhs)))
  where in_scope' = in_scope `extendInScopeSet` bndr

stgCseTopLvl in_scope (StgTopLifted (StgRec eqs))
    = ( in_scope'
      , StgTopLifted (StgRec [ (bndr, stgCseTopLvlRhs in_scope' rhs) | (bndr, rhs) <- eqs ]))
  where in_scope' = in_scope `extendInScopeSetList` [ bndr | (bndr, _) <- eqs ]

stgCseTopLvlRhs :: InScopeSet -> InStgRhs -> OutStgRhs
stgCseTopLvlRhs in_scope (StgRhsClosure ccs info occs upd args body)
    = let body' = stgCseExpr (initEnv in_scope) body
      in  StgRhsClosure ccs info occs upd args body'
stgCseTopLvlRhs _ (StgRhsCon ccs dataCon args)
    = StgRhsCon ccs dataCon args

------------------------------
-- The actual AST traversal --
------------------------------

-- Trivial cases
stgCseExpr :: CseEnv -> InStgExpr -> OutStgExpr
stgCseExpr env (StgApp fun args)
    = StgApp fun' args'
  where fun' = substVar env fun
        args' = substArgs env args
stgCseExpr _ (StgLit lit)
    = StgLit lit
stgCseExpr env (StgOpApp op args tys)
    = StgOpApp op args' tys
  where args' = substArgs env args
stgCseExpr _ (StgLam _ _)
    = pprPanic "stgCseExp" (text "StgLam")
stgCseExpr env (StgTick tick body)
    = let body' = stgCseExpr env body
      in StgTick tick body'
stgCseExpr env (StgCase scrut bndr ty alts)
    = mkStgCase scrut' bndr' ty alts'
  where
    scrut' = stgCseExpr env scrut
    (env1, bndr') = substBndr env bndr
    env2 | StgApp trivial_scrut [] <- scrut' = addTrivCaseBndr bndr trivial_scrut env1
                 -- See Note [Trivial case scrutinee]
         | otherwise                         = env1
    alts' = map (stgCseAlt env2 bndr') alts


-- A constructor application.
-- To be removed by a variable use when found in the CSE environment
stgCseExpr env (StgConApp dataCon args tys)
    | Just bndr' <- envLookup dataCon args' env
    = StgApp bndr' []
    | otherwise
    = StgConApp dataCon args' tys
  where args' = substArgs env args

-- Let bindings
-- The binding might be removed due to CSE (we do not want trivial bindings on
-- the STG level), so use the smart constructor `mkStgLet` to remove the binding
-- if empty.
stgCseExpr env (StgLet binds body)
    = let (binds', env') = stgCseBind env binds
          body' = stgCseExpr env' body
      in mkStgLet StgLet binds' body'
stgCseExpr env (StgLetNoEscape binds body)
    = let (binds', env') = stgCseBind env binds
          body' = stgCseExpr env' body
      in mkStgLet StgLetNoEscape binds' body'

-- Case alternatives
-- Extend the CSE environment
stgCseAlt :: CseEnv -> OutId -> InStgAlt -> OutStgAlt
stgCseAlt env case_bndr (DataAlt dataCon, args, rhs)
    = let (env1, args') = substBndrs env args
          env2 = addDataCon case_bndr dataCon (map StgVarArg args') env1
            -- see note [Case 2: CSEing case binders]
          rhs' = stgCseExpr env2 rhs
      in (DataAlt dataCon, args', rhs')
stgCseAlt env _ (altCon, args, rhs)
    = let (env1, args') = substBndrs env args
          rhs' = stgCseExpr env1 rhs
      in (altCon, args', rhs')

-- Bindings
stgCseBind :: CseEnv -> InStgBinding -> (Maybe OutStgBinding, CseEnv)
stgCseBind env (StgNonRec b e)
    = let (env1, b') = substBndr env b
      in case stgCseRhs env1 b' e of
        (Nothing,      env2) -> (Nothing,                env2)
        (Just (b2,e'), env2) -> (Just (StgNonRec b2 e'), env2)
stgCseBind env (StgRec pairs)
    = let (env1, pairs1) = substPairs env pairs
      in case stgCsePairs env1 pairs1 of
        ([],     env2) -> (Nothing, env2)
        (pairs2, env2) -> (Just (StgRec pairs2), env2)

stgCsePairs :: CseEnv -> [(OutId, InStgRhs)] -> ([(OutId, OutStgRhs)], CseEnv)
stgCsePairs env [] = ([], env)
stgCsePairs env0 ((b,e):pairs)
  = let (pairMB, env1) = stgCseRhs env0 b e
        (pairs', env2) = stgCsePairs env1 pairs
    in (pairMB `mbCons` pairs', env2)
  where
    mbCons = maybe id (:)

-- The RHS of a binding.
-- If it is an constructor application, either short-cut it or extend the environment
stgCseRhs :: CseEnv -> OutId -> InStgRhs -> (Maybe (OutId, OutStgRhs), CseEnv)
stgCseRhs env bndr (StgRhsCon ccs dataCon args)
    | Just other_bndr <- envLookup dataCon args' env
    = let env' = addSubst bndr other_bndr env
      in (Nothing, env')
    | otherwise
    = let env' = addDataCon bndr dataCon args' env
            -- see note [Case 1: CSEing allocated closures]
          pair = (bndr, StgRhsCon ccs dataCon args')
      in (Just pair, env')
  where args' = substArgs env args
stgCseRhs env bndr (StgRhsClosure ccs info occs upd args body)
    = let (env1, args') = substBndrs env args
          env2 = forgetCse env1 -- See note [Free variables of an StgClosure]
          body' = stgCseExpr env2 body
      in (Just (substVar env bndr, StgRhsClosure ccs info occs' upd args' body'), env)
  where occs' = substVars env occs


mkStgCase :: StgExpr -> OutId -> AltType -> [StgAlt] -> StgExpr
mkStgCase scrut bndr ty alts | all isBndr alts = scrut
                             | otherwise       = StgCase scrut bndr ty alts

  where
    -- see Note [All alternatives are the binder]
    isBndr (_, _, StgApp f []) = f == bndr
    isBndr _                   = False


-- Utilities

-- | This function short-cuts let-bindings that are now obsolete
mkStgLet :: (a -> b -> b) -> Maybe a -> b -> b
mkStgLet _      Nothing      body = body
mkStgLet stgLet (Just binds) body = stgLet binds body


{-
Note [All alternatives are the binder]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

When all alternatives simply refer to the case binder, then we do not have
to bother with the case expression at all (#13588). CoreSTG does this as well,
but sometimes, types get into the way:

    newtype T = MkT Int
    f :: (Int, Int) -> (T, Int)
    f (x, y) = (MkT x, y)

Core cannot just turn this into

    f p = p

as this would not be well-typed. But to STG, where MkT is no longer in the way,
we can.

Note [Trivial case scrutinee]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We want to be able to handle nested reconstruction of constructors as in

    nested :: Either Int (Either Int a) -> Either Bool (Either Bool a)
    nested (Right (Right v)) = Right (Right v)
    nested _ = Left True

So if we come across

    case x of r1
      Right a -> case a of r2
              Right b -> let v = Right b
                         in Right v

we first replace v with r2. Next we want to replace Right r2 with r1. But the
ce_conAppMap contains Right a!

Therefore, we add r1 ↦ x to ce_bndrMap when analysing the outer case, and use
this subsitution before looking Right r2 up in ce_conAppMap, and everything
works out.

Note [Free variables of an StgClosure]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
StgClosures (function and thunks) have an explicit list of free variables:

foo [x] =
    let not_a_free_var = Left [x]
    let a_free_var = Right [x]
    let closure = \[x a_free_var] -> \[y] -> bar y (Left [x]) a_free_var
    in closure

If we were to CSE `Left [x]` in the body of `closure` with `not_a_free_var`,
then the list of free variables would be wrong, so for now, we do not CSE
across such a closure, simply because I (Joachim) was not sure about possible
knock-on effects. If deemed safe and worth the slight code complication of
re-calculating this list during or after this pass, this can surely be done.
-}
