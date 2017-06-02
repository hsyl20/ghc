{-# LANGUAGE CPP #-}

module GHC.Utils.CodeGen.Platform.ARM where

#define MACHREGS_NO_REGS 0
#define MACHREGS_arm 1
#include "../../../../includes/CodeGen.Platform.hs"

