{-# OPTIONS_GHC -XDeriveDataTypeable #-}

-- Trac #1935
-- See Note [Superclasses of derived instance] in GHC.IR.Haskell.TypeSystem.Deriving

{-# OPTIONS -fno-warn-redundant-constraints #-}

module Foo where

 import Data.Data

 data Foo a = Foo
    deriving (Data, Typeable)

 data T a = MkT (S a) deriving( Ord )

 instance Num a => Eq (T a)

 data S a = S
 instance Eq (S a)
 instance Ord (S a)


 