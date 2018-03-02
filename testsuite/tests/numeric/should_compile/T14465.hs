{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeInType            #-}

module M where

import Numeric.Natural

-- test Natural literals
one :: Natural
one = fromInteger 1

plusOne :: Natural -> Natural
plusOne n = n + 1

-- test basic constant folding for Natural
twoTimesTwo :: Natural
twoTimesTwo = 2 * 2
