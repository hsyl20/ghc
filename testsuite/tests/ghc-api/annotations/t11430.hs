{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

-- This program must be called with GHC's libdir as the single command line
-- argument.
module Main where

-- import Data.Generics
import Data.Data hiding (Fixity)
import Data.List
import System.IO
import GHC
import GHC.CoreTypes.BasicTypes
import GHC.Config.Flags
import GHC.Data.FastString
import GHC.CoreTypes.ForeignCall
import GHC.Util.Monad
import GHC.Util.Outputable
import GHC.Haskell.Syntax.Declaration
import GHC.Data.Bag (filterBag,isEmptyBag)
import System.Directory (removeFile)
import System.Environment( getArgs )
import qualified Data.Map as Map
import Data.Dynamic ( fromDynamic,Dynamic )

main::IO()
main = do
        [libdir,fileName] <- getArgs
        testOneFile libdir fileName

testOneFile libdir fileName = do
       ((anns,cs),p) <- runGhc (Just libdir) $ do
                        dflags <- getSessionDynFlags
                        setSessionDynFlags dflags
                        let mn =mkModuleName fileName
                        addTarget Target { targetId = TargetModule mn
                                         , targetAllowObjCode = True
                                         , targetContents = Nothing }
                        load LoadAllTargets
                        modSum <- getModSummary mn
                        p <- parseModule modSum
                        return (pm_annotations p,p)

       let tupArgs = gq (pm_parsed_source p)

       putStrLn (intercalate "\n" $ map show tupArgs)
       -- putStrLn (pp tupArgs)
       -- putStrLn (intercalate "\n" [showAnns anns])

    where
     gq ast = everything (++) ([] `mkQ` doFixity
                               `extQ` doRuleDecl
                               `extQ` doHsExpr
                               `extQ` doInline
                              ) ast

     doFixity :: Fixity -> [(String,[String])]
     doFixity (Fixity (SourceText ss) _ _) = [("f",[ss])]

     doRuleDecl :: RuleDecl GhcPs
                -> [(String,[String])]
     doRuleDecl (HsRule _ (ActiveBefore (SourceText ss) _) _ _ _ _ _)
       = [("rb",[ss])]
     doRuleDecl (HsRule _ (ActiveAfter (SourceText ss) _) _ _ _ _ _)
       = [("ra",[ss])]
     doRuleDecl (HsRule _ _ _ _ _ _ _) = []

     doHsExpr :: HsExpr GhcPs -> [(String,[String])]
     doHsExpr (HsTickPragma src (_,_,_) ss _) = [("tp",[show ss])]
     doHsExpr _ = []

     doInline (InlinePragma _ _ _ (ActiveBefore (SourceText ss) _) _)
         = [("ib",[ss])]
     doInline (InlinePragma _ _ _ (ActiveAfter (SourceText ss) _) _)
         = [("ia",[ss])]
     doInline (InlinePragma _ _ _ _ _ ) = []

showAnns anns = "[\n" ++ (intercalate "\n"
   $ map (\((s,k),v)
              -> ("(AK " ++ pp s ++ " " ++ show k ++" = " ++ pp v ++ ")\n"))
   $ Map.toList anns)
    ++ "]\n"

pp a = showPpr unsafeGlobalDynFlags a

-- ---------------------------------------------------------------------

-- Copied from syb for the test


-- | Generic queries of type \"r\",
--   i.e., take any \"a\" and return an \"r\"
--
type GenericQ r = forall a. Data a => a -> r


-- | Make a generic query;
--   start from a type-specific case;
--   return a constant otherwise
--
mkQ :: ( Typeable a
       , Typeable b
       )
    => r
    -> (b -> r)
    -> a
    -> r
(r `mkQ` br) a = case cast a of
                        Just b  -> br b
                        Nothing -> r

-- | Extend a generic query by a type-specific case
extQ :: ( Typeable a
        , Typeable b
        )
     => (a -> q)
     -> (b -> q)
     -> a
     -> q
extQ f g a = maybe (f a) g (cast a)


-- | Summarise all nodes in top-down, left-to-right order
everything :: (r -> r -> r) -> GenericQ r -> GenericQ r

-- Apply f to x to summarise top-level node;
-- use gmapQ to recurse into immediate subterms;
-- use ordinary foldl to reduce list of intermediate results

everything k f x = foldl k (f x) (gmapQ (everything k f) x)
