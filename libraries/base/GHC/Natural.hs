{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  GHC.Natural
-- Copyright   :  (C) 2014 Herbert Valerio Riedel,
--                (C) 2011 Edward Kmett
-- License     :  see libraries/base/LICENSE
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  internal
-- Portability :  non-portable (GHC Extensions)
--
-- The arbitrary-precision 'Natural' number type.
--
-- __Note__: This is an internal GHC module with an API subject to
-- change.  It's recommended use the "Numeric.Natural" module to import
-- the 'Natural' type.
--
-- @since 4.8.0.0
-----------------------------------------------------------------------------
module GHC.Natural
    ( -- * The 'Natural' number type
      --
      -- | __Warning__: The internal implementation of 'Natural'
      -- (i.e. which constructors are available) depends on the
      -- 'Integer' backend used!
      Natural(..)
    , mkNatural
    , isValidNatural
    , smallNatural
      -- * Arithmetic
    , plusNatural
    , minusNatural
    , minusNaturalMaybe
    , timesNatural
    , negateNatural
    , signumNatural
    , quotRemNatural
    , quotNatural
    , remNatural
    , gcdNatural
    , lcmNatural
      -- * Bits
    , andNatural
    , orNatural
    , xorNatural
    , bitNatural
    , testBitNatural
    , popCountNatural
    , shiftLNatural
    , shiftRNatural
      -- * Conversions
    , naturalToInteger
    , naturalToWord
    , naturalToInt
    , naturalFromInteger
    , wordToNatural
    , intToNatural
    , naturalToWordMaybe
      -- * Modular arithmetic
    , powModNatural
    ) where

#include "MachDeps.h"

import GHC.Classes
import GHC.Maybe
import {-# SOURCE #-} GHC.Exception.Type (underflowException, divZeroException)
#if defined(MIN_VERSION_integer_gmp)
import GHC.Integer.GMP.Internals
import GHC.Prim
import GHC.Types
#else
import GHC.Integer ()
#endif

default ()

-- Most high-level operations need to be marked `NOINLINE` as
-- otherwise GHC doesn't recognize them and fails to apply constant
-- folding to `Natural`-typed expression.
--
-- To this end, the CPP hack below allows to write the pseudo-pragma
--
--   {-# CONSTANT_FOLDED plusNatural #-}
--
-- which is simply expanded into a
--
--   {-# NOINLINE plusNatural #-}
--
#define CONSTANT_FOLDED NOINLINE

-------------------------------------------------------------------------------
-- Arithmetic underflow
-------------------------------------------------------------------------------

-- We put them here because they are needed relatively early
-- in the libraries before the Exception type has been defined yet.

{-# NOINLINE underflowError #-}
underflowError :: a
underflowError = raise# underflowException

{-# NOINLINE divZeroError #-}
divZeroError :: a
divZeroError = raise# divZeroException

-------------------------------------------------------------------------------
-- Natural type
-------------------------------------------------------------------------------

#if defined(MIN_VERSION_integer_gmp)
-- TODO: if saturated arithmetic is to used, replace 'underflowError' by '0'

-- | Type representing arbitrary-precision non-negative integers.
--
-- >>> 2^20 :: Natural
-- 1267650600228229401496703205376
--
-- Operations whose result would be negative @'throw' ('Underflow' :: 'ArithException')@,
--
-- >>> -1 :: Natural
-- *** Exception: arithmetic underflow
--
-- @since 4.8.0.0
data Natural = NatS#                 GmpLimb# -- ^ in @[0, maxBound::Word]@
             | NatJ# {-# UNPACK #-} !BigNat   -- ^ in @]maxBound::Word, +inf[@
                                              --
                                              -- __Invariant__: 'NatJ#' is used
                                              -- /iff/ value doesn't fit in
                                              -- 'NatS#' constructor.
                               -- NB: Order of constructors *must*
                               -- coincide with 'Ord' relation
             deriving ( Eq  -- ^ @since 4.8.0.0
                      , Ord -- ^ @since 4.8.0.0
                      )


-- | Test whether all internal invariants are satisfied by 'Natural' value
--
-- This operation is mostly useful for test-suites and/or code which
-- constructs 'Integer' values directly.
--
-- @since 4.8.0.0
isValidNatural :: Natural -> Bool
isValidNatural (NatS# _)  = True
isValidNatural (NatJ# bn) = isTrue# (isValidBigNat# bn)
                            && isTrue# (sizeofBigNat# bn ># 0#)

signumNatural :: Natural -> Natural
signumNatural (NatS# 0##) = NatS# 0##
signumNatural _           = NatS# 1##
{-# CONSTANT_FOLDED signumNatural #-}

negateNatural :: Natural -> Natural
negateNatural (NatS# 0##) = NatS# 0##
negateNatural _           = underflowError
{-# CONSTANT_FOLDED negateNatural #-}

-- | @since 4.10.0.0
naturalFromInteger :: Integer -> Natural
naturalFromInteger (S# i#)
  | isTrue# (i# >=# 0#)     = NatS# (int2Word# i#)
naturalFromInteger (Jp# bn) = bigNatToNatural bn
naturalFromInteger _        = underflowError
{-# CONSTANT_FOLDED naturalFromInteger #-}

-- | Compute greatest common divisor.
gcdNatural :: Natural -> Natural -> Natural
gcdNatural (NatS# 0##) y       = y
gcdNatural x       (NatS# 0##) = x
gcdNatural (NatS# 1##) _       = (NatS# 1##)
gcdNatural _       (NatS# 1##) = (NatS# 1##)
gcdNatural (NatJ# x) (NatJ# y) = bigNatToNatural (gcdBigNat x y)
gcdNatural (NatJ# x) (NatS# y) = NatS# (gcdBigNatWord x y)
gcdNatural (NatS# x) (NatJ# y) = NatS# (gcdBigNatWord y x)
gcdNatural (NatS# x) (NatS# y) = NatS# (gcdWord x y)

-- | compute least common multiplier.
lcmNatural :: Natural -> Natural -> Natural
lcmNatural (NatS# 0##) _ = (NatS# 0##)
lcmNatural _ (NatS# 0##) = (NatS# 0##)
lcmNatural (NatS# 1##) y = y
lcmNatural x (NatS# 1##) = x
lcmNatural x y           = (x `quotNatural` (gcdNatural x y)) `timesNatural` y

----------------------------------------------------------------------------

quotRemNatural :: Natural -> Natural -> (Natural, Natural)
quotRemNatural _ (NatS# 0##) = divZeroError
quotRemNatural n (NatS# 1##) = (n,NatS# 0##)
quotRemNatural n@(NatS# _) (NatJ# _) = (NatS# 0##, n)
quotRemNatural (NatS# n) (NatS# d) = case quotRemWord# n d of
    (# q, r #) -> (NatS# q, NatS# r)
quotRemNatural (NatJ# n) (NatS# d) = case quotRemBigNatWord n d of
    (# q, r #) -> (bigNatToNatural q, NatS# r)
quotRemNatural (NatJ# n) (NatJ# d) = case quotRemBigNat n d of
    (# q, r #) -> (bigNatToNatural q, bigNatToNatural r)
{-# CONSTANT_FOLDED quotRemNatural #-}

quotNatural :: Natural -> Natural -> Natural
quotNatural _       (NatS# 0##) = divZeroError
quotNatural n       (NatS# 1##) = n
quotNatural (NatS# _) (NatJ# _) = NatS# 0##
quotNatural (NatS# n) (NatS# d) = NatS# (quotWord# n d)
quotNatural (NatJ# n) (NatS# d) = bigNatToNatural (quotBigNatWord n d)
quotNatural (NatJ# n) (NatJ# d) = bigNatToNatural (quotBigNat n d)
{-# CONSTANT_FOLDED quotNatural #-}

remNatural :: Natural -> Natural -> Natural
remNatural _         (NatS# 0##) = divZeroError
remNatural _         (NatS# 1##) = NatS# 0##
remNatural n@(NatS# _) (NatJ# _) = n
remNatural   (NatS# n) (NatS# d) = NatS# (remWord# n d)
remNatural   (NatJ# n) (NatS# d) = NatS# (remBigNatWord n d)
remNatural   (NatJ# n) (NatJ# d) = bigNatToNatural (remBigNat n d)
{-# CONSTANT_FOLDED remNatural #-}

-- | @since 4.X.0.0
naturalToInteger :: Natural -> Integer
naturalToInteger (NatS# w)  = wordToInteger w
naturalToInteger (NatJ# bn) = Jp# bn

andNatural :: Natural -> Natural -> Natural
andNatural (NatS# n) (NatS# m) = NatS# (n `and#` m)
andNatural (NatS# n) (NatJ# m) = NatS# (n `and#` bigNatToWord m)
andNatural (NatJ# n) (NatS# m) = NatS# (bigNatToWord n `and#` m)
andNatural (NatJ# n) (NatJ# m) = bigNatToNatural (andBigNat n m)
{-# CONSTANT_FOLDED andNatural #-}

orNatural :: Natural -> Natural -> Natural
orNatural (NatS# n) (NatS# m) = NatS# (n `or#` m)
orNatural (NatS# n) (NatJ# m) = NatJ# (orBigNat (wordToBigNat n) m)
orNatural (NatJ# n) (NatS# m) = NatJ# (orBigNat n (wordToBigNat m))
orNatural (NatJ# n) (NatJ# m) = NatJ# (orBigNat n m)
{-# CONSTANT_FOLDED orNatural #-}

xorNatural :: Natural -> Natural -> Natural
xorNatural (NatS# n) (NatS# m) = NatS# (n `xor#` m)
xorNatural (NatS# n) (NatJ# m) = NatJ# (xorBigNat (wordToBigNat n) m)
xorNatural (NatJ# n) (NatS# m) = NatJ# (xorBigNat n (wordToBigNat m))
xorNatural (NatJ# n) (NatJ# m) = bigNatToNatural (xorBigNat n m)
{-# CONSTANT_FOLDED xorNatural #-}

bitNatural :: Int -> Natural
bitNatural (I# i#)
  | isTrue# (i# <# WORD_SIZE_IN_BITS#) = NatS# (1## `uncheckedShiftL#` i#)
  | True                               = NatJ# (bitBigNat i#)
{-# CONSTANT_FOLDED bitNatural #-}

testBitNatural :: Natural -> Int -> Bool
testBitNatural (NatS# w) (I# i#)
  = isTrue# ((w `and#` (1## `uncheckedShiftL#` i#)) `neWord#` 0##)
testBitNatural (NatJ# bn) (I# i#)
  = testBitBigNat bn i#
{-# CONSTANT_FOLDED testBitNatural #-}

popCountNatural :: Natural -> Int
popCountNatural (NatS# w)  = I# (word2Int# (popCnt# w))
popCountNatural (NatJ# bn) = I# (popCountBigNat bn)
{-# CONSTANT_FOLDED popCountNatural #-}

shiftLNatural :: Natural -> Int -> Natural
shiftLNatural n           (I# 0#) = n
shiftLNatural (NatS# 0##) _       = NatS# 0##
shiftLNatural (NatS# 1##) (I# i#) = NatS# (1## `uncheckedShiftL#` i#)
shiftLNatural (NatS# w) (I# i#)
    = bigNatToNatural (shiftLBigNat (wordToBigNat w) i#)
shiftLNatural (NatJ# bn) (I# i#)
    = bigNatToNatural (shiftLBigNat bn i#)
{-# CONSTANT_FOLDED shiftLNatural #-}

shiftRNatural :: Natural -> Int -> Natural
shiftRNatural n          (I# 0#) = n
shiftRNatural (NatS# w)  (I# i#) = NatS# (w `uncheckedShiftRL#` i#)
shiftRNatural (NatJ# bn) (I# i#) = bigNatToNatural (shiftRBigNat bn i#)
{-# CONSTANT_FOLDED shiftRNatural #-}

----------------------------------------------------------------------------

-- | 'Natural' Addition
plusNatural :: Natural -> Natural -> Natural
plusNatural (NatS# 0##) y         = y
plusNatural x         (NatS# 0##) = x
plusNatural (NatS# x) (NatS# y)
    = case plusWord2# x y of
       (# 0##, l #) -> NatS# l
       (# h,   l #) -> NatJ# (wordToBigNat2 h l)
plusNatural (NatS# x) (NatJ# y) = NatJ# (plusBigNatWord y x)
plusNatural (NatJ# x) (NatS# y) = NatJ# (plusBigNatWord x y)
plusNatural (NatJ# x) (NatJ# y) = NatJ# (plusBigNat     x y)
{-# CONSTANT_FOLDED plusNatural #-}

-- | 'Natural' multiplication
timesNatural :: Natural -> Natural -> Natural
timesNatural _         (NatS# 0##) = NatS# 0##
timesNatural (NatS# 0##) _         = NatS# 0##
timesNatural x         (NatS# 1##) = x
timesNatural (NatS# 1##) y         = y
timesNatural (NatS# x) (NatS# y) = case timesWord2# x y of
    (# 0##, 0## #) -> NatS# 0##
    (# 0##, xy  #) -> NatS# xy
    (# h  , l   #) -> NatJ# (wordToBigNat2 h l)
timesNatural (NatS# x) (NatJ# y) = NatJ# (timesBigNatWord y x)
timesNatural (NatJ# x) (NatS# y) = NatJ# (timesBigNatWord x y)
timesNatural (NatJ# x) (NatJ# y) = NatJ# (timesBigNat     x y)
{-# CONSTANT_FOLDED timesNatural #-}

-- | 'Natural' subtraction. May @'throw' 'Underflow'@.
minusNatural :: Natural -> Natural -> Natural
minusNatural x         (NatS# 0##) = x
minusNatural (NatS# x) (NatS# y) = case subWordC# x y of
    (# l, 0# #) -> NatS# l
    _           -> underflowError
minusNatural (NatS# _) (NatJ# _) = underflowError
minusNatural (NatJ# x) (NatS# y)
    = bigNatToNatural (minusBigNatWord x y)
minusNatural (NatJ# x) (NatJ# y)
    = bigNatToNatural (minusBigNat     x y)
{-# CONSTANT_FOLDED minusNatural #-}

-- | 'Natural' subtraction. Returns 'Nothing's for non-positive results.
--
-- @since 4.8.0.0
minusNaturalMaybe :: Natural -> Natural -> Maybe Natural
minusNaturalMaybe x         (NatS# 0##) = Just x
minusNaturalMaybe (NatS# x) (NatS# y) = case subWordC# x y of
    (# l, 0# #) -> Just (NatS# l)
    _           -> Nothing
minusNaturalMaybe (NatS# _) (NatJ# _) = Nothing
minusNaturalMaybe (NatJ# x) (NatS# y)
    = Just (bigNatToNatural (minusBigNatWord x y))
minusNaturalMaybe (NatJ# x) (NatJ# y)
  | isTrue# (isNullBigNat# res) = Nothing
  | True                        = Just (bigNatToNatural res)
  where
    res = minusBigNat x y

-- | Convert 'BigNat' to 'Natural'.
-- Throws 'Underflow' if passed a 'nullBigNat'.
bigNatToNatural :: BigNat -> Natural
bigNatToNatural bn
  | isTrue# (sizeofBigNat# bn ==# 1#) = NatS# (bigNatToWord bn)
  | isTrue# (isNullBigNat# bn)        = underflowError
  | True                              = NatJ# bn

naturalToBigNat :: Natural -> BigNat
naturalToBigNat (NatS# w#) = wordToBigNat w#
naturalToBigNat (NatJ# bn) = bn

-- | Convert 'Int' to 'Natural'.
-- Throws 'Underflow' when passed a negative 'Int'.
intToNatural :: Int -> Natural
intToNatural (I# i#)
  | isTrue# (i# <# 0#) = underflowError
  | True               = NatS# (int2Word# i#)

naturalToWord :: Natural -> Word
naturalToWord (NatS# w#) = W# w#
naturalToWord (NatJ# bn) = W# (bigNatToWord bn)

naturalToInt :: Natural -> Int
naturalToInt (NatS# w#) = I# (word2Int# w#)
naturalToInt (NatJ# bn) = I# (bigNatToInt bn)

----------------------------------------------------------------------------

-- | Construct 'Natural' value from list of 'Word's.
--
-- This function is used by GHC for constructing 'Natural' literals.
mkNatural :: [Word]  -- ^ value expressed in 32 bit chunks, least
                     --   significant first
          -> Natural
mkNatural [] = NatS# 0##
mkNatural (W# i : is') = smallNatural (i `and#` 0xffffffff##) `orNatural`
                         shiftLNatural (mkNatural is') 31
{-# CONSTANT_FOLDED mkNatural #-}

-- | Should rather be called @wordToNatural@
smallNatural :: Word# -> Natural
smallNatural w# = NatS# w#
{-# CONSTANT_FOLDED smallNatural #-}

#else /* !defined(MIN_VERSION_integer_gmp) */
----------------------------------------------------------------------------
-- Use wrapped 'Integer' as fallback; taken from Edward Kmett's nats package

-- | Type representing arbitrary-precision non-negative integers.
--
-- Operations whose result would be negative
-- @'throw' ('Underflow' :: 'ArithException')@.
--
-- @since 4.8.0.0
newtype Natural = Natural Integer -- ^ __Invariant__: non-negative 'Integer'
                deriving (Eq,Ord,Ix)

-- | Test whether all internal invariants are satisfied by 'Natural' value
--
-- This operation is mostly useful for test-suites and/or code which
-- constructs 'Integer' values directly.
--
-- @since 4.8.0.0
isValidNatural :: Natural -> Bool
isValidNatural (Natural i) = i >= 0

-- | @since 4.8.0.0
instance Read Natural where
    readsPrec d = map (\(n, s) -> (Natural n, s))
                  . filter ((>= 0) . (\(x,_)->x)) . readsPrec d

-- | @since 4.8.0.0
instance Show Natural where
    showsPrec d (Natural i) = showsPrec d i

-- | @since 4.8.0.0
instance Num Natural where
  Natural n + Natural m = Natural (n + m)
  {-# INLINE (+) #-}
  Natural n * Natural m = Natural (n * m)
  {-# INLINE (*) #-}
  Natural n - Natural m | result < 0 = underflowError
                        | otherwise  = Natural result
    where result = n - m
  {-# INLINE (-) #-}
  abs (Natural n) = Natural n
  {-# INLINE abs #-}
  signum (Natural n) = Natural (signum n)
  {-# INLINE signum #-}
  fromInteger = naturalFromInteger
  {-# INLINE fromInteger #-}

-- | @since 4.10.0.0
naturalFromInteger :: Integer -> Natural
naturalFromInteger n
  | n >= 0 = Natural n
  | otherwise = underflowError
{-# INLINE naturalFromInteger #-}

-- | 'Natural' subtraction. Returns 'Nothing's for non-positive results.
--
-- @since 4.8.0.0
minusNaturalMaybe :: Natural -> Natural -> Maybe Natural
minusNaturalMaybe x y
  | x >= y    = Just (x - y)
  | otherwise = Nothing

-- | @since 4.8.0.0
instance Bits Natural where
  Natural n .&. Natural m = Natural (n .&. m)
  {-# INLINE (.&.) #-}
  Natural n .|. Natural m = Natural (n .|. m)
  {-# INLINE (.|.) #-}
  xor (Natural n) (Natural m) = Natural (xor n m)
  {-# INLINE xor #-}
  complement _ = errorWithoutStackTrace "Bits.complement: Natural complement undefined"
  {-# INLINE complement #-}
  shift (Natural n) = Natural . shift n
  {-# INLINE shift #-}
  rotate (Natural n) = Natural . rotate n
  {-# INLINE rotate #-}
  bit = Natural . bit
  {-# INLINE bit #-}
  setBit (Natural n) = Natural . setBit n
  {-# INLINE setBit #-}
  clearBit (Natural n) = Natural . clearBit n
  {-# INLINE clearBit #-}
  complementBit (Natural n) = Natural . complementBit n
  {-# INLINE complementBit #-}
  testBit (Natural n) = testBit n
  {-# INLINE testBit #-}
  bitSizeMaybe _ = Nothing
  {-# INLINE bitSizeMaybe #-}
  bitSize = errorWithoutStackTrace "Natural: bitSize"
  {-# INLINE bitSize #-}
  isSigned _ = False
  {-# INLINE isSigned #-}
  shiftL (Natural n) = Natural . shiftL n
  {-# INLINE shiftL #-}
  shiftR (Natural n) = Natural . shiftR n
  {-# INLINE shiftR #-}
  rotateL (Natural n) = Natural . rotateL n
  {-# INLINE rotateL #-}
  rotateR (Natural n) = Natural . rotateR n
  {-# INLINE rotateR #-}
  popCount (Natural n) = popCount n
  {-# INLINE popCount #-}
  zeroBits = Natural 0

-- | @since 4.8.0.0
instance Real Natural where
  toRational (Natural a) = toRational a
  {-# INLINE toRational #-}

-- | @since 4.8.0.0
instance Enum Natural where
  pred (Natural 0) = errorWithoutStackTrace "Natural.pred: 0"
  pred (Natural n) = Natural (pred n)
  {-# INLINE pred #-}
  succ (Natural n) = Natural (succ n)
  {-# INLINE succ #-}
  fromEnum (Natural n) = fromEnum n
  {-# INLINE fromEnum #-}
  toEnum n | n < 0     = errorWithoutStackTrace "Natural.toEnum: negative"
           | otherwise = Natural (toEnum n)
  {-# INLINE toEnum #-}

  enumFrom     = coerce (enumFrom     :: Integer -> [Integer])
  enumFromThen x y
    | x <= y    = coerce (enumFromThen :: Integer -> Integer -> [Integer]) x y
    | otherwise = enumFromThenTo x y 0

  enumFromTo   = coerce (enumFromTo   :: Integer -> Integer -> [Integer])
  enumFromThenTo
    = coerce (enumFromThenTo :: Integer -> Integer -> Integer -> [Integer])

-- | @since 4.8.0.0
instance Integral Natural where
  quot (Natural a) (Natural b) = Natural (quot a b)
  {-# INLINE quot #-}
  rem (Natural a) (Natural b) = Natural (rem a b)
  {-# INLINE rem #-}
  div (Natural a) (Natural b) = Natural (div a b)
  {-# INLINE div #-}
  mod (Natural a) (Natural b) = Natural (mod a b)
  {-# INLINE mod #-}
  divMod (Natural a) (Natural b) = (Natural q, Natural r)
    where (q,r) = divMod a b
  {-# INLINE divMod #-}
  quotRem (Natural a) (Natural b) = (Natural q, Natural r)
    where (q,r) = quotRem a b
  {-# INLINE quotRem #-}
  toInteger (Natural a) = a
  {-# INLINE toInteger #-}
#endif

-- | Construct 'Natural' from 'Word' value.
--
-- @since 4.8.0.0
wordToNatural :: Word -> Natural
#if defined(MIN_VERSION_integer_gmp)
wordToNatural (W# w#) = NatS# w#
#else
wordToNatural w = Natural (fromIntegral w)
#endif

-- | Try downcasting 'Natural' to 'Word' value.
-- Returns 'Nothing' if value doesn't fit in 'Word'.
--
-- @since 4.8.0.0
naturalToWordMaybe :: Natural -> Maybe Word
#if defined(MIN_VERSION_integer_gmp)
naturalToWordMaybe (NatS# w#) = Just (W# w#)
naturalToWordMaybe (NatJ# _)  = Nothing
#else
naturalToWordMaybe (Natural i)
  | i <= maxw  = Just (fromIntegral i)
  | otherwise  = Nothing
  where
    maxw = toInteger (maxBound :: Word)
#endif

-- | \"@'powModNatural' /b/ /e/ /m/@\" computes base @/b/@ raised to
-- exponent @/e/@ modulo @/m/@.
--
-- @since 4.8.0.0
powModNatural :: Natural -> Natural -> Natural -> Natural
#if defined(MIN_VERSION_integer_gmp)
powModNatural _           _           (NatS# 0##) = divZeroError
powModNatural _           _           (NatS# 1##) = NatS# 0##
powModNatural _           (NatS# 0##) _           = NatS# 1##
powModNatural (NatS# 0##) _           _           = NatS# 0##
powModNatural (NatS# 1##) _           _           = NatS# 1##
powModNatural (NatS# b)   (NatS# e)   (NatS# m)   = NatS# (powModWord b e m)
powModNatural b           e           (NatS# m)
  = NatS# (powModBigNatWord (naturalToBigNat b) (naturalToBigNat e) m)
powModNatural b           e           (NatJ# m)
  = bigNatToNatural (powModBigNat (naturalToBigNat b) (naturalToBigNat e) m)
#else
-- Portable reference fallback implementation
powModNatural _ _ 0 = divZeroError
powModNatural _ _ 1 = 0
powModNatural _ 0 _ = 1
powModNatural 0 _ _ = 0
powModNatural 1 _ _ = 1
powModNatural b0 e0 m = go b0 e0 1
  where
    go !b e !r
      | odd e     = go b' e' (r*b `mod` m)
      | e == 0    = r
      | otherwise = go b' e' r
      where
        b' = b*b `mod` m
        e' = e   `unsafeShiftR` 1 -- slightly faster than "e `div` 2"
#endif
