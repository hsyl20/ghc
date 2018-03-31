{-# LANGUAGE CPP #-}

-- | Built-in numeric literals
module PrelNumLits
   ( litNumDesc
   , builtinLitNumDescs
   , numDescInt
   , numDescInt64
   , numDescWord
   , numDescWord64
   , numDescInteger
   , numDescNatural
   , litNumConvertNames
   )
where

#include "HsVersions.h"

import GhcPrelude
import Id
import MkId
import Literal
import PrimOp
import DynFlags
import PrelNames
import Name
import Platform

import Data.Int
import Data.Word

-----------------------------------------------------------------
-- Numeric literals
-----------------------------------------------------------------

-- | Get the description of a numeric literal type
litNumDesc :: DynFlags -> LitNumType -> LitNumDesc
litNumDesc dflags nt = case nt of
   LitNumInt     -> numDescInt dflags
   LitNumWord    -> numDescWord dflags
   LitNumInt64   -> numDescInt64
   LitNumWord64  -> numDescWord64
   LitNumInteger -> numDescInteger
   LitNumNatural -> numDescNatural

builtinLitNumDescs :: DynFlags -> [LitNumDesc]
builtinLitNumDescs dflags =
   [ numDescInt dflags
   , numDescWord dflags
   , numDescInt64
   , numDescWord64
   , numDescInteger
   , numDescNatural
   ]

numDescInteger :: LitNumDesc
numDescInteger = LitNumDesc
  { litNumSigned       = True
  , litNumOverflowMode = OverflowFail -- we don't have bounds anyway...
  , litNumMake         = swap mkLitInteger
  , litNumBitsize      = Nothing
  , litNumMinBound     = Nothing
  , litNumMaxBound     = Nothing
  , litNumNameAdd      = Just plusIntegerName
  , litNumNameSub      = Just minusIntegerName
  , litNumNameNegate   = Just negateIntegerName
  , litNumNameMul      = Just timesIntegerName
  , litNumNameQuot     = Just quotIntegerName
  , litNumNameRem      = Just remIntegerName
  , litNumNameQuotRem  = Just quotRemIntegerName
  , litNumNameAnd      = Just andIntegerName
  , litNumNameOr       = Just orIntegerName
  , litNumNameXor      = Just xorIntegerName
  , litNumNameNot      = Just complementIntegerName
  , litNumNameEq       = Just eqIntegerPrimName
  , litNumNameNe       = Just neqIntegerPrimName
  , litNumNameGt       = Just gtIntegerPrimName
  , litNumNameGe       = Just geIntegerPrimName
  , litNumNameLt       = Just ltIntegerPrimName
  , litNumNameLe       = Just leIntegerPrimName
  }

numDescNatural :: LitNumDesc
numDescNatural = LitNumDesc
  { litNumSigned       = False
  , litNumOverflowMode = OverflowFail
  , litNumMake         = swap mkLitNatural
  , litNumBitsize      = Nothing
  , litNumMinBound     = Just 0
  , litNumMaxBound     = Nothing
  , litNumNameAdd      = Just plusNaturalName
  , litNumNameSub      = Just minusNaturalName
  , litNumNameNegate   = Nothing
  , litNumNameMul      = Just timesNaturalName
  , litNumNameQuot     = Nothing  -- TODO: add missing names
  , litNumNameRem      = Nothing
  , litNumNameQuotRem  = Nothing
  , litNumNameAnd      = Nothing
  , litNumNameOr       = Nothing
  , litNumNameXor      = Nothing
  , litNumNameNot      = Nothing
  , litNumNameEq       = Nothing
  , litNumNameNe       = Nothing
  , litNumNameGt       = Nothing
  , litNumNameGe       = Nothing
  , litNumNameLt       = Nothing
  , litNumNameLe       = Nothing
  }

numDescInt :: DynFlags -> LitNumDesc
numDescInt dflags = LitNumDesc
  { litNumSigned       = True
  , litNumOverflowMode = OverflowWrap
  , litNumMake         = const (mkLitInt dflags)
  , litNumBitsize      = Just (fromIntegral 
                           (8*platformWordSize (targetPlatform dflags)))
  , litNumMinBound     = Just (tARGET_MIN_INT dflags)
  , litNumMaxBound     = Just (tARGET_MAX_INT dflags)
  , litNumNameAdd      = Just (idName (mkPrimOpId IntAddOp))
  , litNumNameSub      = Just (idName (mkPrimOpId IntSubOp))
  , litNumNameNegate   = Just (idName (mkPrimOpId IntNegOp))
  , litNumNameMul      = Just (idName (mkPrimOpId IntMulOp))
  , litNumNameQuot     = Just (idName (mkPrimOpId IntQuotOp))
  , litNumNameRem      = Just (idName (mkPrimOpId IntRemOp))
  , litNumNameQuotRem  = Nothing
  , litNumNameAnd      = Just (idName (mkPrimOpId AndIOp))
  , litNumNameOr       = Just (idName (mkPrimOpId OrIOp))
  , litNumNameXor      = Just (idName (mkPrimOpId XorIOp))
  , litNumNameNot      = Just (idName (mkPrimOpId NotIOp))
  , litNumNameEq       = Just (idName (mkPrimOpId IntEqOp))
  , litNumNameNe       = Just (idName (mkPrimOpId IntNeOp))
  , litNumNameGt       = Just (idName (mkPrimOpId IntGtOp))
  , litNumNameGe       = Just (idName (mkPrimOpId IntGeOp))
  , litNumNameLt       = Just (idName (mkPrimOpId IntLtOp))
  , litNumNameLe       = Just (idName (mkPrimOpId IntLeOp))
  }

numDescWord :: DynFlags -> LitNumDesc
numDescWord dflags = LitNumDesc
  { litNumSigned       = False
  , litNumOverflowMode = OverflowWrap
  , litNumMake         = const (mkLitWord dflags)
  , litNumBitsize      = Just (fromIntegral
                           (8*platformWordSize (targetPlatform dflags)))
  , litNumMinBound     = Just 0
  , litNumMaxBound     = Just (tARGET_MAX_WORD dflags)
  , litNumNameAdd      = Just (idName (mkPrimOpId WordAddOp))
  , litNumNameSub      = Just (idName (mkPrimOpId WordSubOp))
  , litNumNameNegate   = Nothing
  , litNumNameMul      = Just (idName (mkPrimOpId WordMulOp))
  , litNumNameQuot     = Just (idName (mkPrimOpId WordQuotOp))
  , litNumNameRem      = Just (idName (mkPrimOpId WordRemOp))
  , litNumNameQuotRem  = Nothing
  , litNumNameAnd      = Just (idName (mkPrimOpId AndOp))
  , litNumNameOr       = Just (idName (mkPrimOpId OrOp))
  , litNumNameXor      = Just (idName (mkPrimOpId XorOp))
  , litNumNameNot      = Just (idName (mkPrimOpId NotOp))
  , litNumNameEq       = Just (idName (mkPrimOpId WordEqOp))
  , litNumNameNe       = Just (idName (mkPrimOpId WordNeOp))
  , litNumNameGt       = Just (idName (mkPrimOpId WordGtOp))
  , litNumNameGe       = Just (idName (mkPrimOpId WordGeOp))
  , litNumNameLt       = Just (idName (mkPrimOpId WordLtOp))
  , litNumNameLe       = Just (idName (mkPrimOpId WordLeOp))
  }

numDescInt64 :: LitNumDesc
numDescInt64 = LitNumDesc
  { litNumSigned       = True
  , litNumOverflowMode = OverflowWrap
  , litNumMake         = const mkLitInt64
  , litNumBitsize      = Just 64
  , litNumMinBound     = Just (toInteger (minBound :: Int64))
  , litNumMaxBound     = Just (toInteger (maxBound :: Int64))
  , litNumNameAdd      = Nothing
  , litNumNameSub      = Nothing
  , litNumNameNegate   = Nothing
  , litNumNameMul      = Nothing
  , litNumNameQuot     = Nothing
  , litNumNameRem      = Nothing
  , litNumNameQuotRem  = Nothing
  , litNumNameAnd      = Nothing
  , litNumNameOr       = Nothing
  , litNumNameXor      = Nothing
  , litNumNameNot      = Nothing
  , litNumNameEq       = Nothing
  , litNumNameNe       = Nothing
  , litNumNameGt       = Nothing
  , litNumNameGe       = Nothing
  , litNumNameLt       = Nothing
  , litNumNameLe       = Nothing
  }

numDescWord64 :: LitNumDesc
numDescWord64 = LitNumDesc
  { litNumSigned       = False
  , litNumOverflowMode = OverflowWrap
  , litNumMake         = const mkLitWord64
  , litNumBitsize      = Just 64
  , litNumMinBound     = Just 0
  , litNumMaxBound     = Just (toInteger (maxBound :: Word64))
  , litNumNameAdd      = Nothing
  , litNumNameSub      = Nothing
  , litNumNameNegate   = Nothing
  , litNumNameMul      = Nothing
  , litNumNameQuot     = Nothing
  , litNumNameRem      = Nothing
  , litNumNameQuotRem  = Nothing
  , litNumNameAnd      = Nothing
  , litNumNameOr       = Nothing
  , litNumNameXor      = Nothing
  , litNumNameNot      = Nothing
  , litNumNameEq       = Nothing
  , litNumNameNe       = Nothing
  , litNumNameGt       = Nothing
  , litNumNameGe       = Nothing
  , litNumNameLt       = Nothing
  , litNumNameLe       = Nothing
  }

-- | Conversion functions (word2int, etc.)
litNumConvertNames :: DynFlags -> [(Name,LitNumDesc,LitNumDesc)]
litNumConvertNames dflags =
   [ (idName (mkPrimOpId Word2IntOp), numDescWord dflags , numDescInt  dflags)
   , (idName (mkPrimOpId Int2WordOp), numDescInt dflags  , numDescWord dflags)
   , (smallIntegerName              , numDescInt dflags  , numDescInteger)
   , (wordToIntegerName             , numDescWord dflags , numDescInteger)
   , (int64ToIntegerName            , numDescInt64       , numDescInteger)
   , (word64ToIntegerName           , numDescWord64      , numDescInteger)
   , (integerToWordName             , numDescInteger     , numDescWord dflags)
   , (integerToIntName              , numDescInteger     , numDescInt dflags)
   , (integerToWord64Name           , numDescInteger     , numDescWord64)
   , (integerToInt64Name            , numDescInteger     , numDescInt64)
   ]


