{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE NoImplicitPrelude #-}

module GHC.Exception.Type
  ( SomeException
  , divZeroException, overflowException, ratioZeroDenomException, underflowException
  ) where

data SomeException
divZeroException, overflowException, ratioZeroDenomException  :: SomeException
underflowException :: SomeException
