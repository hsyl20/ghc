{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -O #-}

-- This one showed up a bug that required type refinement in
-- GHC.Interface.TypeCheck
-- See the call to coreRefineTys in GHC.Interface.TypeCheck
--
-- Tests for bug: http://ghc.haskell.org/trac/ghc/ticket/685

module ShouldCompile where

import Gadt17_help ( TernOp (..), applyTernOp )

liftTernOpObs :: TernOp a b c d -> a -> b -> c ->  d
liftTernOpObs op x y z = applyTernOp op x y z
