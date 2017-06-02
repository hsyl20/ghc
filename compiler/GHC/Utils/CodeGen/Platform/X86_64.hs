{-# LANGUAGE CPP #-}

module GHC.Utils.CodeGen.Platform.X86_64 where

#define MACHREGS_NO_REGS 0
#define MACHREGS_x86_64 1
#include "../../../../includes/CodeGen.Platform.hs"

