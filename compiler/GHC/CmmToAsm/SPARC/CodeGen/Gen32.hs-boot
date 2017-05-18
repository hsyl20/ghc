module GHC.CmmToAsm.SPARC.CodeGen.Gen32 (
        getSomeReg,
        getRegister
)

where

import GHC.CmmToAsm.SPARC.CodeGen.Base
import GHC.CmmToAsm.Monad
import GHC.CmmToAsm.Register

import GHC.Cmm.Syntax

getSomeReg  :: CmmExpr -> NatM (Reg, InstrBlock)
getRegister :: CmmExpr -> NatM Register
