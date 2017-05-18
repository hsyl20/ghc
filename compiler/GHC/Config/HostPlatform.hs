{-# LANGUAGE CPP #-}

-------------------------------------------------------------------------------
--
-- | Platform constants
--
-- (c) The University of Glasgow 2013
--
-------------------------------------------------------------------------------

module GHC.Config.HostPlatform (PlatformConstants(..)) where

import GhcPrelude

-- Produced by deriveConstants
#include "GHCConstantsHaskellType.hs"

