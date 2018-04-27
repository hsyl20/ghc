{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecordWildCards #-}

-- | Coalesce memory to memory operations
module CmmCoalesceOpt
   ( cmmCoalesce
   )
where

import GhcPrelude

import Cmm
import DynFlags
import SMRep
import CmmUtils
import Hoopl.Block

cmmCoalesce :: DynFlags -> CmmDecl -> CmmDecl
cmmCoalesce dflags  (CmmProc h l regs g)
   = CmmProc h l regs (cmmCoalesceGraph dflags g)
cmmCoalesce _dflags x                    = x

cmmCoalesceGraph :: DynFlags -> CmmGraph -> CmmGraph
cmmCoalesceGraph dflags g@(CmmGraph entry _)
   = ofBlockList entry . fmap (cmmCoalesceBlock dflags) . toBlockList $ g

cmmCoalesceBlock :: DynFlags -> CmmBlock -> CmmBlock
cmmCoalesceBlock dflags block = blockJoin entry middle' last
   where
      (entry, middle, last) = blockSplit block
      middle' = blockFromList . coalesce dflags . blockToList $ middle


data CoalesceState = CoalesceState
   { coalSrcReg     :: !CmmReg
   , coalDstReg     :: !CmmReg
   , coalSrcOff     :: {-# UNPACK #-} !ByteOff
   , coalDstOff     :: {-# UNPACK #-} !ByteOff
   , coalSize       :: {-# UNPACK #-} !Int
   , coalSingleNode :: !(Maybe (CmmNode O O)) -- ^ Saved single node
   }

coalesce :: DynFlags -> [CmmNode O O] -> [CmmNode O O]
coalesce dflags block = go block Nothing
   where
   emitStuff :: CoalesceState -> CmmNode O O
   emitStuff CoalesceState{..} = case coalSingleNode of
     Just e  -> e
     Nothing -> CmmUnsafeForeignCall
                   (PrimTarget (MO_Memcpy 1)) -- alignment, de we know better?
                   [] -- no result
                   [ (cmmRegOffB dflags coalDstReg coalDstOff) 
                   , (cmmRegOffB dflags coalSrcReg coalSrcOff)
                   , (CmmLit (CmmInt (fromIntegral coalSize) (wordWidth dflags)))
                   ]

   go :: [CmmNode O O] -> Maybe CoalesceState -> [CmmNode O O]
   go [] Nothing  = []
   go [] (Just s) = [emitStuff s]
   go (x@(CmmStore u (CmmLoad v ty)):xs) s = case (u,v) of
      (CmmRegOff dstReg dstOff, CmmRegOff srcReg srcOff) ->
         tryUpdateState x xs dstReg dstOff srcReg srcOff ty s
      (CmmReg dstReg          , CmmRegOff srcReg srcOff) ->
         tryUpdateState x xs dstReg 0 srcReg srcOff ty s
      (CmmRegOff dstReg dstOff, CmmReg srcReg) ->
         tryUpdateState x xs dstReg dstOff srcReg 0 ty s
      (CmmReg dstReg          , CmmReg srcReg) ->
         tryUpdateState x xs dstReg 0 srcReg 0 ty s

   go (x:xs) (Just s) = emitStuff s : x : go xs Nothing
   go (x:xs) Nothing  = x : go xs Nothing
     
   tryUpdateState x xs dstReg dstOff srcReg srcOff ty state
      = case state of
         Nothing -> go xs (Just (CoalesceState srcReg dstReg srcOff dstOff (widthInBytes(typeWidth ty)) (Just x)))
         Just s
            | srcReg == coalSrcReg s
            , dstReg == coalDstReg s
            , srcOff == coalSrcOff s + coalSize s
            , dstOff == coalDstOff s + coalSize s
            -> go xs (Just (s
                  { coalSize       = coalSize s + widthInBytes (typeWidth ty)
                  , coalSingleNode = Nothing
                  }))
            | otherwise
            -> emitStuff s : go (x:xs) Nothing

   -- STEP 1: detect CmmStore (CmmRegOff ..) (CmmLoad (CmmRegOff ..) ty)
   -- STEP 2: handle CmmRegOff and CmmReg params... (equivalent to CmmRegOff r 0)
   -- STEP 3: handle intermediate variables
