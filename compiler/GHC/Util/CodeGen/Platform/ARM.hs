{-# LANGUAGE CPP #-}

module GHC.Util.CodeGen.Platform.ARM where

import GhcPrelude

#define MACHREGS_NO_REGS 0
#define MACHREGS_arm 1
#include "../../../../../includes/CodeGen.Platform.hs"
