{-# LANGUAGE CPP #-}

module GHC.Utils.CodeGen.Platform.X86 where

#define MACHREGS_NO_REGS 0
#define MACHREGS_i386 1
#include "../../../../includes/CodeGen.Platform.hs"

