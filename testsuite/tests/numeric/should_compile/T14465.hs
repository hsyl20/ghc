{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeInType            #-}

module M where

import Numeric.Natural
import GHC.Natural

-- test Natural literals
one :: Natural
one = fromInteger 1

plusOne :: Natural -> Natural
plusOne n = n + 1

-- a built-in rule should convert this unfolding into a Natural literal in Core
ten :: Natural
ten = wordToNatural 10

-- test basic constant folding for Natural
twoTimesTwo :: Natural
twoTimesTwo = 2 * 2
