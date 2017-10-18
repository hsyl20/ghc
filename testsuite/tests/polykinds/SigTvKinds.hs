{-# LANGUAGE PolyKinds #-}

module SigTvKinds where

data T (a :: k1) x = MkT (S a ())
data S (b :: k2) y = MkS (T b ())
  -- tests GHC.TypeCheck.TypeDecl.no_sig_tv
