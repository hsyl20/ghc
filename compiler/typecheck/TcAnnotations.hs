{-
(c) The University of Glasgow 2006
(c) The AQUA Project, Glasgow University, 1993-1998

\section[TcAnnotations]{Typechecking annotations}
-}

{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}

module TcAnnotations ( tcAnnotations, annCtxt ) where

import {-# SOURCE #-} TcSplice ( runAnnotation )
import Module
import DynFlags
import Control.Monad ( when )

import HsSyn
import GHC.Types.Annotations
import Name
import TcRnMonad
import SrcLoc
import Outputable

-- Some platforms don't support the external interpreter, and
-- compilation on those platforms shouldn't fail just due to
-- annotations
#ifndef GHCI
tcAnnotations :: [LAnnDecl Name] -> TcM [Annotation]
tcAnnotations anns = do
  dflags <- getDynFlags
  case gopt Opt_ExternalInterpreter dflags of
    True  -> tcAnnotations' anns
    False -> warnAnns anns
warnAnns :: [LAnnDecl Name] -> TcM [Annotation]
--- No GHCI; emit a warning (not an error) and ignore. cf Trac #4268
warnAnns [] = return []
warnAnns anns@(L loc _ : _)
  = do { setSrcSpan loc $ addWarnTc NoReason $
             (text "Ignoring ANN annotation" <> plural anns <> comma
             <+> text "because this is a stage-1 compiler without -fexternal-interpreter or doesn't support GHCi")
       ; return [] }
#else
tcAnnotations :: [LAnnDecl Name] -> TcM [Annotation]
tcAnnotations = tcAnnotations'
#endif

tcAnnotations' :: [LAnnDecl Name] -> TcM [Annotation]
tcAnnotations' anns = mapM tcAnnotation anns

tcAnnotation :: LAnnDecl Name -> TcM Annotation
tcAnnotation (L loc ann@(HsAnnotation _ provenance expr)) = do
    -- Work out what the full target of this annotation was
    mod <- getModule
    let target = annProvenanceToTarget mod provenance

    -- Run that annotation and construct the full Annotation data structure
    setSrcSpan loc $ addErrCtxt (annCtxt ann) $ do
      -- See #10826 -- Annotations allow one to bypass Safe Haskell.
      dflags <- getDynFlags
      when (safeLanguageOn dflags) $ failWithTc safeHsErr
      runAnnotation target expr
    where
      safeHsErr = vcat [ text "Annotations are not compatible with Safe Haskell."
                  , text "See https://ghc.haskell.org/trac/ghc/ticket/10826" ]

annProvenanceToTarget :: Module -> AnnProvenance Name -> AnnTarget Name
annProvenanceToTarget _   (ValueAnnProvenance (L _ name)) = NamedTarget name
annProvenanceToTarget _   (TypeAnnProvenance (L _ name))  = NamedTarget name
annProvenanceToTarget mod ModuleAnnProvenance             = ModuleTarget mod

annCtxt :: (OutputableBndrId id) => AnnDecl id -> SDoc
annCtxt ann
  = hang (text "In the annotation:") 2 (ppr ann)
