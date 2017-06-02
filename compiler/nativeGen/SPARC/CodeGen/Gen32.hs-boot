
module SPARC.CodeGen.Gen32 (
        getSomeReg,
        getRegister
)

where

import SPARC.CodeGen.Base
import GHC.Compilers.CmmToAsm.Monad
import Reg

import GHC.IR.Cmm.Syntax

getSomeReg  :: CmmExpr -> NatM (Reg, InstrBlock)
getRegister :: CmmExpr -> NatM Register
