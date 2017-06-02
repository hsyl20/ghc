{-# LANGUAGE GADTs #-}
module GHC.IR.Cmm.Transform.Sinking (
     cmmSink
  ) where

import GHC.IR.Cmm
import GHC.IR.Cmm.Transform.ConstantFolding
import GHC.IR.Cmm.Analyse.Liveness
import GHC.IR.Cmm.Utils
import GHC.IR.Cmm.Transform.Dataflow
import CodeGen.Platform
import GHC.Platform (isARM, platformArch)

import GHC.Config.Flags
import GHC.Data.Unique.FiniteMap
import GHC.IR.Cmm.PrettyPrint ()

import Data.List (partition)
import qualified Data.Set as Set
import Data.Maybe

-- -----------------------------------------------------------------------------
-- Sinking and inlining

-- This is an optimisation pass that
--  (a) moves assignments closer to their uses, to reduce register pressure
--  (b) pushes assignments into a single branch of a conditional if possible
--  (c) inlines assignments to registers that are mentioned only once
--  (d) discards dead assignments
--
-- This tightens up lots of register-heavy code.  It is particularly
-- helpful in the Cmm generated by the Stg->Cmm code generator, in
-- which every function starts with a copyIn sequence like:
--
--    x1 = R1
--    x2 = Sp[8]
--    x3 = Sp[16]
--    if (Sp - 32 < SpLim) then L1 else L2
--
-- we really want to push the x1..x3 assignments into the L2 branch.
--
-- Algorithm:
--
--  * Start by doing liveness analysis.
--
--  * Keep a list of assignments A; earlier ones may refer to later ones.
--    Currently we only sink assignments to local registers, because we don't
--    have liveness information about global registers.
--
--  * Walk forwards through the graph, look at each node N:
--
--    * If it is a dead assignment, i.e. assignment to a register that is
--      not used after N, discard it.
--
--    * Try to inline based on current list of assignments
--      * If any assignments in A (1) occur only once in N, and (2) are
--        not live after N, inline the assignment and remove it
--        from A.
--
--      * If an assignment in A is cheap (RHS is local register), then
--        inline the assignment and keep it in A in case it is used afterwards.
--
--      * Otherwise don't inline.
--
--    * If N is assignment to a local register pick up the assignment
--      and add it to A.
--
--    * If N is not an assignment to a local register:
--      * remove any assignments from A that conflict with N, and
--        place them before N in the current block.  We call this
--        "dropping" the assignments.
--
--      * An assignment conflicts with N if it:
--        - assigns to a register mentioned in N
--        - mentions a register assigned by N
--        - reads from memory written by N
--      * do this recursively, dropping dependent assignments
--
--    * At an exit node:
--      * drop any assignments that are live on more than one successor
--        and are not trivial
--      * if any successor has more than one predecessor (a join-point),
--        drop everything live in that successor. Since we only propagate
--        assignments that are not dead at the successor, we will therefore
--        eliminate all assignments dead at this point. Thus analysis of a
--        join-point will always begin with an empty list of assignments.
--
--
-- As a result of above algorithm, sinking deletes some dead assignments
-- (transitively, even).  This isn't as good as removeDeadAssignments,
-- but it's much cheaper.

-- -----------------------------------------------------------------------------
-- things that we aren't optimising very well yet.
--
-- -----------
-- (1) From GHC's FastString.hashStr:
--
--  s2ay:
--      if ((_s2an::I64 == _s2ao::I64) >= 1) goto c2gn; else goto c2gp;
--  c2gn:
--      R1 = _s2au::I64;
--      call (I64[Sp])(R1) args: 8, res: 0, upd: 8;
--  c2gp:
--      _s2cO::I64 = %MO_S_Rem_W64(%MO_UU_Conv_W8_W64(I8[_s2aq::I64 + (_s2an::I64 << 0)]) + _s2au::I64 * 128,
--                                 4091);
--      _s2an::I64 = _s2an::I64 + 1;
--      _s2au::I64 = _s2cO::I64;
--      goto s2ay;
--
-- a nice loop, but we didn't eliminate the silly assignment at the end.
-- See Note [dependent assignments], which would probably fix this.
-- This is #8336 on Trac.
--
-- -----------
-- (2) From stg_atomically_frame in PrimOps.cmm
--
-- We have a diamond control flow:
--
--     x = ...
--       |
--      / \
--     A   B
--      \ /
--       |
--    use of x
--
-- Now x won't be sunk down to its use, because we won't push it into
-- both branches of the conditional.  We certainly do have to check
-- that we can sink it past all the code in both A and B, but having
-- discovered that, we could sink it to its use.
--

-- -----------------------------------------------------------------------------

type Assignment = (LocalReg, CmmExpr, AbsMem)
  -- Assignment caches AbsMem, an abstraction of the memory read by
  -- the RHS of the assignment.

type Assignments = [Assignment]
  -- A sequence of assignments; kept in *reverse* order
  -- So the list [ x=e1, y=e2 ] means the sequence of assignments
  --     y = e2
  --     x = e1

cmmSink :: DynFlags -> CmmGraph -> CmmGraph
cmmSink dflags graph = ofBlockList (g_entry graph) $ sink mapEmpty $ blocks
  where
  liveness = cmmLocalLiveness dflags graph
  getLive l = mapFindWithDefault Set.empty l liveness

  blocks = postorderDfs graph

  join_pts = findJoinPoints blocks

  sink :: LabelMap Assignments -> [CmmBlock] -> [CmmBlock]
  sink _ [] = []
  sink sunk (b:bs) =
    -- pprTrace "sink" (ppr lbl) $
    blockJoin first final_middle final_last : sink sunk' bs
    where
      lbl = entryLabel b
      (first, middle, last) = blockSplit b

      succs = successors last

      -- Annotate the middle nodes with the registers live *after*
      -- the node.  This will help us decide whether we can inline
      -- an assignment in the current node or not.
      live = Set.unions (map getLive succs)
      live_middle = gen_kill dflags last live
      ann_middles = annotate dflags live_middle (blockToList middle)

      -- Now sink and inline in this block
      (middle', assigs) = walk dflags ann_middles (mapFindWithDefault [] lbl sunk)
      fold_last = constantFoldNode dflags last
      (final_last, assigs') = tryToInline dflags live fold_last assigs

      -- We cannot sink into join points (successors with more than
      -- one predecessor), so identify the join points and the set
      -- of registers live in them.
      (joins, nonjoins) = partition (`mapMember` join_pts) succs
      live_in_joins = Set.unions (map getLive joins)

      -- We do not want to sink an assignment into multiple branches,
      -- so identify the set of registers live in multiple successors.
      -- This is made more complicated because when we sink an assignment
      -- into one branch, this might change the set of registers that are
      -- now live in multiple branches.
      init_live_sets = map getLive nonjoins
      live_in_multi live_sets r =
         case filter (Set.member r) live_sets of
           (_one:_two:_) -> True
           _ -> False

      -- Now, drop any assignments that we will not sink any further.
      (dropped_last, assigs'') = dropAssignments dflags drop_if init_live_sets assigs'

      drop_if a@(r,rhs,_) live_sets = (should_drop, live_sets')
          where
            should_drop =  conflicts dflags a final_last
                        || not (isTrivial dflags rhs) && live_in_multi live_sets r
                        || r `Set.member` live_in_joins

            live_sets' | should_drop = live_sets
                       | otherwise   = map upd live_sets

            upd set | r `Set.member` set = set `Set.union` live_rhs
                    | otherwise          = set

            live_rhs = foldRegsUsed dflags extendRegSet emptyRegSet rhs

      final_middle = foldl blockSnoc middle' dropped_last

      sunk' = mapUnion sunk $
                 mapFromList [ (l, filterAssignments dflags (getLive l) assigs'')
                             | l <- succs ]

{- TODO: enable this later, when we have some good tests in place to
   measure the effect and tune it.

-- small: an expression we don't mind duplicating
isSmall :: CmmExpr -> Bool
isSmall (CmmReg (CmmLocal _)) = True  --
isSmall (CmmLit _) = True
isSmall (CmmMachOp (MO_Add _) [x,y]) = isTrivial x && isTrivial y
isSmall (CmmRegOff (CmmLocal _) _) = True
isSmall _ = False
-}

--
-- We allow duplication of trivial expressions: registers (both local and
-- global) and literals.
--
isTrivial :: DynFlags -> CmmExpr -> Bool
isTrivial _ (CmmReg (CmmLocal _)) = True
isTrivial dflags (CmmReg (CmmGlobal r)) = -- see Note [Inline GlobalRegs?]
  if isARM (platformArch (targetPlatform dflags))
  then True -- CodeGen.Platform.ARM does not have globalRegMaybe
  else isJust (globalRegMaybe (targetPlatform dflags) r)
  -- GlobalRegs that are loads from BaseReg are not trivial
isTrivial _ (CmmLit _) = True
isTrivial _ _          = False

--
-- annotate each node with the set of registers live *after* the node
--
annotate :: DynFlags -> LocalRegSet -> [CmmNode O O] -> [(LocalRegSet, CmmNode O O)]
annotate dflags live nodes = snd $ foldr ann (live,[]) nodes
  where ann n (live,nodes) = (gen_kill dflags n live, (live,n) : nodes)

--
-- Find the blocks that have multiple successors (join points)
--
findJoinPoints :: [CmmBlock] -> LabelMap Int
findJoinPoints blocks = mapFilter (>1) succ_counts
 where
  all_succs = concatMap successors blocks

  succ_counts :: LabelMap Int
  succ_counts = foldr (\l -> mapInsertWith (+) l 1) mapEmpty all_succs

--
-- filter the list of assignments to remove any assignments that
-- are not live in a continuation.
--
filterAssignments :: DynFlags -> LocalRegSet -> Assignments -> Assignments
filterAssignments dflags live assigs = reverse (go assigs [])
  where go []             kept = kept
        go (a@(r,_,_):as) kept | needed    = go as (a:kept)
                               | otherwise = go as kept
           where
              needed = r `Set.member` live
                       || any (conflicts dflags a) (map toNode kept)
                       --  Note that we must keep assignments that are
                       -- referred to by other assignments we have
                       -- already kept.

-- -----------------------------------------------------------------------------
-- Walk through the nodes of a block, sinking and inlining assignments
-- as we go.
--
-- On input we pass in a:
--    * list of nodes in the block
--    * a list of assignments that appeared *before* this block and
--      that are being sunk.
--
-- On output we get:
--    * a new block
--    * a list of assignments that will be placed *after* that block.
--

walk :: DynFlags
     -> [(LocalRegSet, CmmNode O O)]    -- nodes of the block, annotated with
                                        -- the set of registers live *after*
                                        -- this node.

     -> Assignments                     -- The current list of
                                        -- assignments we are sinking.
                                        -- Earlier assignments may refer
                                        -- to later ones.

     -> ( Block CmmNode O O             -- The new block
        , Assignments                   -- Assignments to sink further
        )

walk dflags nodes assigs = go nodes emptyBlock assigs
 where
   go []               block as = (block, as)
   go ((live,node):ns) block as
    | shouldDiscard node live           = go ns block as
       -- discard dead assignment
    | Just a <- shouldSink dflags node2 = go ns block (a : as1)
    | otherwise                         = go ns block' as'
    where
      node1 = constantFoldNode dflags node

      (node2, as1) = tryToInline dflags live node1 as

      (dropped, as') = dropAssignmentsSimple dflags
                          (\a -> conflicts dflags a node2) as1

      block' = foldl blockSnoc block dropped `blockSnoc` node2


--
-- Heuristic to decide whether to pick up and sink an assignment
-- Currently we pick up all assignments to local registers.  It might
-- be profitable to sink assignments to global regs too, but the
-- liveness analysis doesn't track those (yet) so we can't.
--
shouldSink :: DynFlags -> CmmNode e x -> Maybe Assignment
shouldSink dflags (CmmAssign (CmmLocal r) e) | no_local_regs = Just (r, e, exprMem dflags e)
  where no_local_regs = True -- foldRegsUsed (\_ _ -> False) True e
shouldSink _ _other = Nothing

--
-- discard dead assignments.  This doesn't do as good a job as
-- removeDeadAssignments, because it would need multiple passes
-- to get all the dead code, but it catches the common case of
-- superfluous reloads from the stack that the stack allocator
-- leaves behind.
--
-- Also we catch "r = r" here.  You might think it would fall
-- out of inlining, but the inliner will see that r is live
-- after the instruction and choose not to inline r in the rhs.
--
shouldDiscard :: CmmNode e x -> LocalRegSet -> Bool
shouldDiscard node live
   = case node of
       CmmAssign r (CmmReg r') | r == r' -> True
       CmmAssign (CmmLocal r) _ -> not (r `Set.member` live)
       _otherwise -> False


toNode :: Assignment -> CmmNode O O
toNode (r,rhs,_) = CmmAssign (CmmLocal r) rhs

dropAssignmentsSimple :: DynFlags -> (Assignment -> Bool) -> Assignments
                      -> ([CmmNode O O], Assignments)
dropAssignmentsSimple dflags f = dropAssignments dflags (\a _ -> (f a, ())) ()

dropAssignments :: DynFlags -> (Assignment -> s -> (Bool, s)) -> s -> Assignments
                -> ([CmmNode O O], Assignments)
dropAssignments dflags should_drop state assigs
 = (dropped, reverse kept)
 where
   (dropped,kept) = go state assigs [] []

   go _ []             dropped kept = (dropped, kept)
   go state (assig : rest) dropped kept
      | conflict  = go state' rest (toNode assig : dropped) kept
      | otherwise = go state' rest dropped (assig:kept)
      where
        (dropit, state') = should_drop assig state
        conflict = dropit || any (conflicts dflags assig) dropped


-- -----------------------------------------------------------------------------
-- Try to inline assignments into a node.
-- This also does constant folding for primpops, since
-- inlining opens up opportunities for doing so.

tryToInline
   :: DynFlags
   -> LocalRegSet               -- set of registers live after this
                                -- node.  We cannot inline anything
                                -- that is live after the node, unless
                                -- it is small enough to duplicate.
   -> CmmNode O x               -- The node to inline into
   -> Assignments               -- Assignments to inline
   -> (
        CmmNode O x             -- New node
      , Assignments             -- Remaining assignments
      )

tryToInline dflags live node assigs = go usages node [] assigs
 where
  usages :: UniqFM Int -- Maps each LocalReg to a count of how often it is used
  usages = foldLocalRegsUsed dflags addUsage emptyUFM node

  go _usages node _skipped [] = (node, [])

  go usages node skipped (a@(l,rhs,_) : rest)
   | cannot_inline           = dont_inline
   | occurs_none             = discard  -- Note [discard during inlining]
   | occurs_once             = inline_and_discard
   | isTrivial dflags rhs    = inline_and_keep
   | otherwise               = dont_inline
   where
        inline_and_discard = go usages' inl_node skipped rest
          where usages' = foldLocalRegsUsed dflags addUsage usages rhs

        discard = go usages node skipped rest

        dont_inline        = keep node  -- don't inline the assignment, keep it
        inline_and_keep    = keep inl_node -- inline the assignment, keep it

        keep node' = (final_node, a : rest')
          where (final_node, rest') = go usages' node' (l:skipped) rest
                usages' = foldLocalRegsUsed dflags (\m r -> addToUFM m r 2)
                                            usages rhs
                -- we must not inline anything that is mentioned in the RHS
                -- of a binding that we have already skipped, so we set the
                -- usages of the regs on the RHS to 2.

        cannot_inline = skipped `regsUsedIn` rhs -- Note [dependent assignments]
                        || l `elem` skipped
                        || not (okToInline dflags rhs node)

        l_usages = lookupUFM usages l
        l_live   = l `elemRegSet` live

        occurs_once = not l_live && l_usages == Just 1
        occurs_none = not l_live && l_usages == Nothing

        inl_node = case mapExpDeep inl_exp node of
                     -- See Note [Improving conditionals]
                     CmmCondBranch (CmmMachOp (MO_Ne w) args)
                                   ti fi l
                           -> CmmCondBranch (cmmMachOpFold dflags (MO_Eq w) args)
                                            fi ti l
                     node' -> node'

        inl_exp :: CmmExpr -> CmmExpr
        -- inl_exp is where the inlining actually takes place!
        inl_exp (CmmReg    (CmmLocal l'))     | l == l' = rhs
        inl_exp (CmmRegOff (CmmLocal l') off) | l == l'
                    = cmmOffset dflags rhs off
                    -- re-constant fold after inlining
        inl_exp (CmmMachOp op args) = cmmMachOpFold dflags op args
        inl_exp other = other

{- Note [Improving conditionals]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Given
  CmmCondBranch ((a >## b) != 1) t f
where a,b, are Floats, the constant folder /cannot/ turn it into
  CmmCondBranch (a <=## b) t f
because comparison on floats are not invertible
(see CmmMachOp.maybeInvertComparison).

What we want instead is simply to reverse the true/false branches thus
  CmmCondBranch ((a >## b) != 1) t f
-->
  CmmCondBranch (a >## b) f t

And we do that right here in tryToInline, just as we do cmmMachOpFold.
-}

-- Note [dependent assignments]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~
--
-- If our assignment list looks like
--
--    [ y = e,  x = ... y ... ]
--
-- We cannot inline x.  Remember this list is really in reverse order,
-- so it means  x = ... y ...; y = e
--
-- Hence if we inline x, the outer assignment to y will capture the
-- reference in x's right hand side.
--
-- In this case we should rename the y in x's right-hand side,
-- i.e. change the list to [ y = e, x = ... y1 ..., y1 = y ]
-- Now we can go ahead and inline x.
--
-- For now we do nothing, because this would require putting
-- everything inside UniqSM.
--
-- One more variant of this (#7366):
--
--   [ y = e, y = z ]
--
-- If we don't want to inline y = e, because y is used many times, we
-- might still be tempted to inline y = z (because we always inline
-- trivial rhs's).  But of course we can't, because y is equal to e,
-- not z.

-- Note [discard during inlining]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- Opportunities to discard assignments sometimes appear after we've
-- done some inlining.  Here's an example:
--
--      x = R1;
--      y = P64[x + 7];
--      z = P64[x + 15];
--      /* z is dead */
--      R1 = y & (-8);
--
-- The x assignment is trivial, so we inline it in the RHS of y, and
-- keep both x and y.  z gets dropped because it is dead, then we
-- inline y, and we have a dead assignment to x.  If we don't notice
-- that x is dead in tryToInline, we end up retaining it.

addUsage :: UniqFM Int -> LocalReg -> UniqFM Int
addUsage m r = addToUFM_C (+) m r 1

regsUsedIn :: [LocalReg] -> CmmExpr -> Bool
regsUsedIn [] _ = False
regsUsedIn ls e = wrapRecExpf f e False
  where f (CmmReg (CmmLocal l))      _ | l `elem` ls = True
        f (CmmRegOff (CmmLocal l) _) _ | l `elem` ls = True
        f _ z = z

-- we don't inline into CmmUnsafeForeignCall if the expression refers
-- to global registers.  This is a HACK to avoid global registers
-- clashing with C argument-passing registers, really the back-end
-- ought to be able to handle it properly, but currently neither PprC
-- nor the NCG can do it.  See Note [Register parameter passing]
-- See also GHC.Compilers.StgToCmm.ForeignCall:load_args_into_temps.
okToInline :: DynFlags -> CmmExpr -> CmmNode e x -> Bool
okToInline dflags expr node@(CmmUnsafeForeignCall{}) =
    not (globalRegistersConflict dflags expr node)
okToInline _ _ _ = True

-- -----------------------------------------------------------------------------

-- | @conflicts (r,e) node@ is @False@ if and only if the assignment
-- @r = e@ can be safely commuted past statement @node@.
conflicts :: DynFlags -> Assignment -> CmmNode O x -> Bool
conflicts dflags (r, rhs, addr) node

  -- (1) node defines registers used by rhs of assignment. This catches
  -- assignments and all three kinds of calls. See Note [Sinking and calls]
  | globalRegistersConflict dflags rhs node                       = True
  | localRegistersConflict  dflags rhs node                       = True

  -- (2) node uses register defined by assignment
  | foldRegsUsed dflags (\b r' -> r == r' || b) False node        = True

  -- (3) a store to an address conflicts with a read of the same memory
  | CmmStore addr' e <- node
  , memConflicts addr (loadAddr dflags addr' (cmmExprWidth dflags e)) = True

  -- (4) an assignment to Hp/Sp conflicts with a heap/stack read respectively
  | HeapMem    <- addr, CmmAssign (CmmGlobal Hp) _ <- node        = True
  | StackMem   <- addr, CmmAssign (CmmGlobal Sp) _ <- node        = True
  | SpMem{}    <- addr, CmmAssign (CmmGlobal Sp) _ <- node        = True

  -- (5) foreign calls clobber heap: see Note [Foreign calls clobber heap]
  | CmmUnsafeForeignCall{} <- node, memConflicts addr AnyMem      = True

  -- (6) native calls clobber any memory
  | CmmCall{} <- node, memConflicts addr AnyMem                   = True

  -- (7) otherwise, no conflict
  | otherwise = False

-- Returns True if node defines any global registers that are used in the
-- Cmm expression
globalRegistersConflict :: DynFlags -> CmmExpr -> CmmNode e x -> Bool
globalRegistersConflict dflags expr node =
    foldRegsDefd dflags (\b r -> b || regUsedIn dflags (CmmGlobal r) expr)
                 False node

-- Returns True if node defines any local registers that are used in the
-- Cmm expression
localRegistersConflict :: DynFlags -> CmmExpr -> CmmNode e x -> Bool
localRegistersConflict dflags expr node =
    foldRegsDefd dflags (\b r -> b || regUsedIn dflags (CmmLocal  r) expr)
                 False node

-- Note [Sinking and calls]
-- ~~~~~~~~~~~~~~~~~~~~~~~~
--
-- We have three kinds of calls: normal (CmmCall), safe foreign (CmmForeignCall)
-- and unsafe foreign (CmmUnsafeForeignCall). We perform sinking pass after
-- stack layout (see Note [Sinking after stack layout]) which leads to two
-- invariants related to calls:
--
--   a) during stack layout phase all safe foreign calls are turned into
--      unsafe foreign calls (see Note [Lower safe foreign calls]). This
--      means that we will never encounter CmmForeignCall node when running
--      sinking after stack layout
--
--   b) stack layout saves all variables live across a call on the stack
--      just before making a call (remember we are not sinking assignments to
--      stack):
--
--       L1:
--          x = R1
--          P64[Sp - 16] = L2
--          P64[Sp - 8]  = x
--          Sp = Sp - 16
--          call f() returns L2
--       L2:
--
--      We will attempt to sink { x = R1 } but we will detect conflict with
--      { P64[Sp - 8]  = x } and hence we will drop { x = R1 } without even
--      checking whether it conflicts with { call f() }. In this way we will
--      never need to check any assignment conflicts with CmmCall. Remember
--      that we still need to check for potential memory conflicts.
--
-- So the result is that we only need to worry about CmmUnsafeForeignCall nodes
-- when checking conflicts (see Note [Unsafe foreign calls clobber caller-save registers]).
-- This assumption holds only when we do sinking after stack layout. If we run
-- it before stack layout we need to check for possible conflicts with all three
-- kinds of calls. Our `conflicts` function does that by using a generic
-- foldRegsDefd and foldRegsUsed functions defined in DefinerOfRegs and
-- UserOfRegs typeclasses.
--

-- An abstraction of memory read or written.
data AbsMem
  = NoMem            -- no memory accessed
  | AnyMem           -- arbitrary memory
  | HeapMem          -- definitely heap memory
  | StackMem         -- definitely stack memory
  | SpMem            -- <size>[Sp+n]
       {-# UNPACK #-} !Int
       {-# UNPACK #-} !Int

-- Having SpMem is important because it lets us float loads from Sp
-- past stores to Sp as long as they don't overlap, and this helps to
-- unravel some long sequences of
--    x1 = [Sp + 8]
--    x2 = [Sp + 16]
--    ...
--    [Sp + 8]  = xi
--    [Sp + 16] = xj
--
-- Note that SpMem is invalidated if Sp is changed, but the definition
-- of 'conflicts' above handles that.

-- ToDo: this won't currently fix the following commonly occurring code:
--    x1 = [R1 + 8]
--    x2 = [R1 + 16]
--    ..
--    [Hp - 8] = x1
--    [Hp - 16] = x2
--    ..

-- because [R1 + 8] and [Hp - 8] are both HeapMem.  We know that
-- assignments to [Hp + n] do not conflict with any other heap memory,
-- but this is tricky to nail down.  What if we had
--
--   x = Hp + n
--   [x] = ...
--
--  the store to [x] should be "new heap", not "old heap".
--  Furthermore, you could imagine that if we started inlining
--  functions in Cmm then there might well be reads of heap memory
--  that was written in the same basic block.  To take advantage of
--  non-aliasing of heap memory we will have to be more clever.

-- Note [Foreign calls clobber heap]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
--
-- It is tempting to say that foreign calls clobber only
-- non-heap/stack memory, but unfortunately we break this invariant in
-- the RTS.  For example, in stg_catch_retry_frame we call
-- stmCommitNestedTransaction() which modifies the contents of the
-- TRec it is passed (this actually caused incorrect code to be
-- generated).
--
-- Since the invariant is true for the majority of foreign calls,
-- perhaps we ought to have a special annotation for calls that can
-- modify heap/stack memory.  For now we just use the conservative
-- definition here.
--
-- Some CallishMachOp imply a memory barrier e.g. AtomicRMW and
-- therefore we should never float any memory operations across one of
-- these calls.


bothMems :: AbsMem -> AbsMem -> AbsMem
bothMems NoMem    x         = x
bothMems x        NoMem     = x
bothMems HeapMem  HeapMem   = HeapMem
bothMems StackMem StackMem     = StackMem
bothMems (SpMem o1 w1) (SpMem o2 w2)
  | o1 == o2  = SpMem o1 (max w1 w2)
  | otherwise = StackMem
bothMems SpMem{}  StackMem  = StackMem
bothMems StackMem SpMem{}   = StackMem
bothMems _         _        = AnyMem

memConflicts :: AbsMem -> AbsMem -> Bool
memConflicts NoMem      _          = False
memConflicts _          NoMem      = False
memConflicts HeapMem    StackMem   = False
memConflicts StackMem   HeapMem    = False
memConflicts SpMem{}    HeapMem    = False
memConflicts HeapMem    SpMem{}    = False
memConflicts (SpMem o1 w1) (SpMem o2 w2)
  | o1 < o2   = o1 + w1 > o2
  | otherwise = o2 + w2 > o1
memConflicts _         _         = True

exprMem :: DynFlags -> CmmExpr -> AbsMem
exprMem dflags (CmmLoad addr w)  = bothMems (loadAddr dflags addr (typeWidth w)) (exprMem dflags addr)
exprMem dflags (CmmMachOp _ es)  = foldr bothMems NoMem (map (exprMem dflags) es)
exprMem _      _                 = NoMem

loadAddr :: DynFlags -> CmmExpr -> Width -> AbsMem
loadAddr dflags e w =
  case e of
   CmmReg r       -> regAddr dflags r 0 w
   CmmRegOff r i  -> regAddr dflags r i w
   _other | regUsedIn dflags (CmmGlobal Sp) e -> StackMem
          | otherwise -> AnyMem

regAddr :: DynFlags -> CmmReg -> Int -> Width -> AbsMem
regAddr _      (CmmGlobal Sp) i w = SpMem i (widthInBytes w)
regAddr _      (CmmGlobal Hp) _ _ = HeapMem
regAddr _      (CmmGlobal CurrentTSO) _ _ = HeapMem -- important for PrimOps
regAddr dflags r _ _ | isGcPtrType (cmmRegType dflags r) = HeapMem -- yay! GCPtr pays for itself
regAddr _      _ _ _ = AnyMem

{-
Note [Inline GlobalRegs?]

Should we freely inline GlobalRegs?

Actually it doesn't make a huge amount of difference either way, so we
*do* currently treat GlobalRegs as "trivial" and inline them
everywhere, but for what it's worth, here is what I discovered when I
(SimonM) looked into this:

Common sense says we should not inline GlobalRegs, because when we
have

  x = R1

the register allocator will coalesce this assignment, generating no
code, and simply record the fact that x is bound to $rbx (or
whatever).  Furthermore, if we were to sink this assignment, then the
range of code over which R1 is live increases, and the range of code
over which x is live decreases.  All things being equal, it is better
for x to be live than R1, because R1 is a fixed register whereas x can
live in any register.  So we should neither sink nor inline 'x = R1'.

However, not inlining GlobalRegs can have surprising
consequences. e.g. (cgrun020)

  c3EN:
      _s3DB::P64 = R1;
      _c3ES::P64 = _s3DB::P64 & 7;
      if (_c3ES::P64 >= 2) goto c3EU; else goto c3EV;
  c3EU:
      _s3DD::P64 = P64[_s3DB::P64 + 6];
      _s3DE::P64 = P64[_s3DB::P64 + 14];
      I64[Sp - 8] = c3F0;
      R1 = _s3DE::P64;
      P64[Sp] = _s3DD::P64;

inlining the GlobalReg gives:

  c3EN:
      if (R1 & 7 >= 2) goto c3EU; else goto c3EV;
  c3EU:
      I64[Sp - 8] = c3F0;
      _s3DD::P64 = P64[R1 + 6];
      R1 = P64[R1 + 14];
      P64[Sp] = _s3DD::P64;

but if we don't inline the GlobalReg, instead we get:

      _s3DB::P64 = R1;
      if (_s3DB::P64 & 7 >= 2) goto c3EU; else goto c3EV;
  c3EU:
      I64[Sp - 8] = c3F0;
      R1 = P64[_s3DB::P64 + 14];
      P64[Sp] = P64[_s3DB::P64 + 6];

This looks better - we managed to inline _s3DD - but in fact it
generates an extra reg-reg move:

.Lc3EU:
        movq $c3F0_info,-8(%rbp)
        movq %rbx,%rax
        movq 14(%rbx),%rbx
        movq 6(%rax),%rax
        movq %rax,(%rbp)

because _s3DB is now live across the R1 assignment, we lost the
benefit of coalescing.

Who is at fault here?  Perhaps if we knew that _s3DB was an alias for
R1, then we would not sink a reference to _s3DB past the R1
assignment.  Or perhaps we *should* do that - we might gain by sinking
it, despite losing the coalescing opportunity.

Sometimes not inlining global registers wins by virtue of the rule
about not inlining into arguments of a foreign call, e.g. (T7163) this
is what happens when we inlined F1:

      _s3L2::F32 = F1;
      _c3O3::F32 = %MO_F_Mul_W32(F1, 10.0 :: W32);
      (_s3L7::F32) = call "ccall" arg hints:  []  result hints:  [] rintFloat(_c3O3::F32);

but if we don't inline F1:

      (_s3L7::F32) = call "ccall" arg hints:  []  result hints:  [] rintFloat(%MO_F_Mul_W32(_s3L2::F32,
                                                                                            10.0 :: W32));
-}
