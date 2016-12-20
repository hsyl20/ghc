module Report
   ( Report
   , defaultReport
   )
where

import {-# SOURCE #-} DynFlags (DynFlags)

data Report

defaultReport :: DynFlags -> Report -> IO ()

