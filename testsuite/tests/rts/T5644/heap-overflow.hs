module Main where

import GHC.Utils
import ManyQueue

main = do
  runTest testManyQueue'1P3C 
  runTest testManyQueue'1P1C 
