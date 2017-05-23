-----------------------------------------------------------------------------
-- A Parser monad with access to the 'DynFlags'.
--
-- The 'P' monad  only has access to the subset of of 'DynFlags'
-- required for parsing Haskell.

-- The parser for C-- requires access to a lot more of the 'DynFlags',
-- so 'PD' provides access to 'DynFlags' via a 'HasDynFlags' instance.
-----------------------------------------------------------------------------
{-# LANGUAGE CPP #-}
module CmmMonad (
    PD(..)
  , liftP
  ) where

import Control.Monad
#if __GLASGOW_HASKELL__ > 710
import qualified Control.Monad.Fail as MonadFail
#endif

import GHC.Config.Flags
import Lexer

newtype PD a = PD { unPD :: DynFlags -> PState -> ParseResult a }

instance Functor PD where
  fmap = liftM

instance Applicative PD where
  pure = returnPD
  (<*>) = ap

instance Monad PD where
  (>>=) = thenPD
  fail = failPD

#if __GLASGOW_HASKELL__ > 710
instance MonadFail.MonadFail PD where
  fail = failPD
#endif

liftP :: P a -> PD a
liftP (P f) = PD $ \_ s -> f s

returnPD :: a -> PD a
returnPD = liftP . return

thenPD :: PD a -> (a -> PD b) -> PD b
(PD m) `thenPD` k = PD $ \d s ->
        case m d s of
                POk s1 a         -> unPD (k a) d s1
                PFailed warnFn span err -> PFailed warnFn span err

failPD :: String -> PD a
failPD = liftP . fail

instance HasDynFlags PD where
   getDynFlags = PD $ \d s -> POk s d
