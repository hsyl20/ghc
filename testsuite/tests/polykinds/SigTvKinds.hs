{-# LANGUAGE PolyKinds #-}

module SigTvKinds where

data T (a :: k1) x = MkT (S a ())
data S (b :: k2) y = MkS (T b ())
  -- tests GHC.Haskell.TypeCheck.TypeAndClassDeclaration.no_sig_tv
