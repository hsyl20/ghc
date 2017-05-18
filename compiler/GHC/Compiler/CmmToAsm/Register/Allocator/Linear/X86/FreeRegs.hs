
-- | Free regs map for i386
module GHC.Compiler.CmmToAsm.Register.Allocator.Linear.X86.FreeRegs
where

import GHC.Compiler.CmmToAsm.X86.Regs
import GHC.Compiler.CmmToAsm.Register.Class
import GHC.Compiler.CmmToAsm.Register
import GHC.Utils.Panic
import GHC.Utils.Platform

import Data.Word
import Data.Bits
import Data.Foldable (foldl')

newtype FreeRegs = FreeRegs Word32
    deriving Show

noFreeRegs :: FreeRegs
noFreeRegs = FreeRegs 0

releaseReg :: RealReg -> FreeRegs -> FreeRegs
releaseReg (RealRegSingle n) (FreeRegs f)
    = FreeRegs (f .|. (1 `shiftL` n))

releaseReg _ _
    = panic "CmmToAsm.Register.Allocator.Linear.X86.FreeRegs.releaseReg: no reg"

initFreeRegs :: Platform -> FreeRegs
initFreeRegs platform
        = foldl' (flip releaseReg) noFreeRegs (allocatableRegs platform)

getFreeRegs :: Platform -> RegClass -> FreeRegs -> [RealReg] -- lazily
getFreeRegs platform cls (FreeRegs f) = go f 0

  where go 0 _ = []
        go n m
          | n .&. 1 /= 0 && classOfRealReg platform (RealRegSingle m) == cls
          = RealRegSingle m : (go (n `shiftR` 1) $! (m+1))

          | otherwise
          = go (n `shiftR` 1) $! (m+1)
        -- ToDo: there's no point looking through all the integer registers
        -- in order to find a floating-point one.

allocateReg :: RealReg -> FreeRegs -> FreeRegs
allocateReg (RealRegSingle r) (FreeRegs f)
  = FreeRegs (f .&. complement (1 `shiftL` r))

allocateReg _ _
  = panic "CmmToAsm.Register.Allocator.Linear.X86.FreeRegs.allocateReg: no reg"

