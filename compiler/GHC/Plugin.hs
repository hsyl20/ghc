{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}

-- | This module is not used by GHC itself.  Rather, it exports all of
-- the functions and types you are likely to need when writing a
-- plugin for GHC. So authors of plugins can probably get away simply
-- with saying "import GHC.Plugin".
--
-- Particularly interesting modules for plugin writers include
-- "GHC.IR.Core.Syntax" and "GHC.IR.Core.Pipeline".
module GHC.Plugin
   ( module GHC.Plugin.Types
   , module GHC.Entity.RdrName
   , module GHC.Entity.OccName
   , module GHC.Entity.Name
   , module GHC.Entity.Var
   , module GHC.Entity.Id
   , module GHC.Entity.Id.Info
   , module GHC.IR.Core.Pipeline
   , module GHC.IR.Core.Syntax
   , module GHC.Entity.Literal
   , module GHC.Entity.DataConstructor
   , module GHC.IR.Core.Utils
   , module GHC.IR.Core.Syntax.Make
   , module GHC.IR.Core.Analyser.FreeVars
   , module GHC.IR.Core.Transformer.Substitution
   , module GHC.IR.Core.Transformer.Rules
   , module GHC.Entity.Annotation
   , module GHC.Config.Flags
   , module GHC.Packages
   , module GHC.Entity.Module
   , module GHC.Entity.Type
   , module GHC.Entity.TypeConstructor
   , module GHC.Entity.Coercion
   , module GHC.Builtin.Types
   , module GHC.Entity.Types
   , module GHC.Entity.BasicTypes
   , module GHC.Entity.Var.Set
   , module GHC.Entity.Var.Environment
   , module GHC.Entity.Name.Set
   , module GHC.Entity.Name.Environment
   , module GHC.Entity.Unique.Set
   , module GHC.Entity.Unique.FiniteMap
   , module GHC.Data.FiniteMap
   , module GHC.Utils
   , module GHC.Serialized
   , module GHC.Entity.SrcLoc
   , module GHC.Utils.Outputable
   , module GHC.Entity.Unique.Supply
   , module GHC.Entity.Unique
   , module GHC.Data.FastString
   )
where

-- Plugin stuff itself
import GHC.Plugin.Types

-- Variable naming
import GHC.Entity.RdrName
import GHC.Entity.OccName  hiding  ( varName {- conflicts with Var.varName -} )
import GHC.Entity.Name     hiding  ( varName {- reexport from OccName,
                                                conflicts with Var.varName -} )
import GHC.Entity.Var
import GHC.Entity.Id       hiding
         {- all three conflict with Var -}
         ( lazySetIdInfo, setIdExported, setIdNotExported )
import GHC.Entity.Id.Info

-- Core
import GHC.IR.Core.Pipeline
import GHC.IR.Core.Syntax
import GHC.Entity.Literal
import GHC.Entity.DataConstructor
import GHC.IR.Core.Utils
import GHC.IR.Core.Syntax.Make
import GHC.IR.Core.Analyser.FreeVars
import GHC.IR.Core.Transformer.Substitution hiding
         -- These names are also exported by Type
         ( substTyVarBndr, substCoVarBndr, extendCvSubst )

-- Core "extras"
import GHC.IR.Core.Transformer.Rules
import GHC.Entity.Annotation

-- Pipeline-related stuff
import GHC.Config.Flags
import GHC.Packages

-- Important GHC types
import GHC.Entity.Module
import GHC.Entity.Type     hiding
                {- conflict with GHC.IR.Core.Transformer.Substitution -}
                ( substTy, extendTvSubst, extendTvSubstList, isInScope )
import GHC.Entity.Coercion hiding
                {- conflict with GHC.IR.Core.Transformer.Substitution -}
                ( substCo )
import GHC.Entity.TypeConstructor
import GHC.Builtin.Types
import GHC.Entity.Types
import GHC.Entity.BasicTypes hiding
               {- conflicts with GHC.Packages.Version -}
               ( Version )

-- Collections and maps
import GHC.Entity.Var.Set
import GHC.Entity.Var.Environment
import GHC.Entity.Name.Set
import GHC.Entity.Name.Environment
import GHC.Entity.Unique.Set
import GHC.Entity.Unique.FiniteMap
-- Conflicts with UniqFM:
--import LazyUniqFM
import GHC.Data.FiniteMap

-- Common utilities
import GHC.Utils
import GHC.Serialized
import GHC.Entity.SrcLoc
import GHC.Utils.Outputable
import GHC.Entity.Unique.Supply
import GHC.Entity.Unique           ( Unique, Uniquable(..) )
import GHC.Data.FastString
