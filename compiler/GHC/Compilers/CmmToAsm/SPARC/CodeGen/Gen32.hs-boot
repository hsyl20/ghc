
module GHC.Compilers.CmmToAsm.SPARC.CodeGen.Gen32 (
        getSomeReg,
        getRegister
)

where

import GHC.Compilers.CmmToAsm.SPARC.CodeGen.Base
import GHC.Compilers.CmmToAsm.Monad
import GHC.Compilers.CmmToAsm.Register

import GHC.IR.Cmm.Syntax

getSomeReg  :: CmmExpr -> NatM (Reg, InstrBlock)
getRegister :: CmmExpr -> NatM Register
