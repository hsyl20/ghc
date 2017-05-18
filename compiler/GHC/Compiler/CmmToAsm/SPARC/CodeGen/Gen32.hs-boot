module GHC.Compiler.CmmToAsm.SPARC.CodeGen.Gen32 (
        getSomeReg,
        getRegister
)

where

import GHC.Compiler.CmmToAsm.SPARC.CodeGen.Base
import GHC.Compiler.CmmToAsm.Monad
import GHC.Compiler.CmmToAsm.Register

import GHC.Cmm.Syntax

getSomeReg  :: CmmExpr -> NatM (Reg, InstrBlock)
getRegister :: CmmExpr -> NatM Register
