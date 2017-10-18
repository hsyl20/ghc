{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}

-- | This module is not used by GHC itself.  Rather, it exports all of
-- the functions and types you are likely to need when writing a
-- plugin for GHC. So authors of plugins can probably get away simply
-- with saying "import GHC.Plugin".
--
-- Particularly interesting modules for plugin writers include
-- "GHC.Core.Syntax" and "GHC.Core.Monad".
module GHC.Plugin
   ( module GHC.Plugin.Types
   , module GHC.CoreTypes.RdrName
   , module GHC.CoreTypes.OccName
   , module GHC.CoreTypes.Name
   , module GHC.CoreTypes.Var
   , module GHC.CoreTypes.Id
   , module GHC.CoreTypes.Id.Info
   , module GHC.Core.Monad
   , module GHC.Core.Syntax
   , module GHC.CoreTypes.Literal
   , module GHC.CoreTypes.DataCon
   , module GHC.Core.Util
   , module GHC.Core.Syntax.Make
   , module GHC.Core.FreeVars
   , module GHC.Core.Subst
   , module GHC.Core.Rules
   , module GHC.CoreTypes.Annotation
   , module GHC.Config.Flags
   , module GHC.Packages
   , module GHC.CoreTypes.Module
   , module GHC.CoreTypes.Type
   , module GHC.CoreTypes.TyCon
   , module GHC.CoreTypes.Coercion
   , module GHC.Builtin.Types
   , module GHC.CoreTypes.Base
   , module GHC.CoreTypes.BasicTypes
   , module GHC.CoreTypes.Var.Set
   , module GHC.CoreTypes.Var.Environment
   , module GHC.CoreTypes.Name.Set
   , module GHC.CoreTypes.Name.Environment
   , module GHC.Data.UniqueSet
   , module GHC.Data.UniqueFM
   , module GHC.Data.FiniteMap
   , module GHC.Util
   , module GHC.Serialized
   , module GHC.CoreTypes.SrcLoc
   , module GHC.Util.Outputable
   , module GHC.Data.UniqueSupply
   , module GHC.CoreTypes.Unique
   , module GHC.Data.FastString
   )
where

-- Plugin stuff itself
import GHC.Plugin.Types

-- Variable naming
import GHC.CoreTypes.RdrName
import GHC.CoreTypes.OccName  hiding  ( varName {- conflicts with Var.varName -} )
import GHC.CoreTypes.Name     hiding  ( varName {- reexport from OccName,
                                                conflicts with Var.varName -} )
import GHC.CoreTypes.Var
import GHC.CoreTypes.Id       hiding
         {- all three conflict with Var -}
         ( lazySetIdInfo, setIdExported, setIdNotExported )
import GHC.CoreTypes.Id.Info

-- Core
import GHC.Core.Monad
import GHC.Core.Syntax
import GHC.CoreTypes.Literal
import GHC.CoreTypes.DataCon
import GHC.Core.Util
import GHC.Core.Syntax.Make
import GHC.Core.FreeVars
import GHC.Core.Subst hiding
         -- These names are also exported by Type
         ( substTyVarBndr, substCoVarBndr, extendCvSubst )

-- Core "extras"
import GHC.Core.Rules
import GHC.CoreTypes.Annotation

-- Pipeline-related stuff
import GHC.Config.Flags
import GHC.Packages

-- Important GHC types
import GHC.CoreTypes.Module
import GHC.CoreTypes.Type     hiding
                {- conflict with GHC.Core.Subst -}
                ( substTy, extendTvSubst, extendTvSubstList, isInScope )
import GHC.CoreTypes.Coercion hiding
                {- conflict with GHC.Core.Subst -}
                ( substCo )
import GHC.CoreTypes.TyCon
import GHC.Builtin.Types
import GHC.CoreTypes.Base
import GHC.CoreTypes.BasicTypes hiding
               {- conflicts with GHC.Packages.Version -}
               ( Version )

-- Collections and maps
import GHC.CoreTypes.Var.Set
import GHC.CoreTypes.Var.Environment
import GHC.CoreTypes.Name.Set
import GHC.CoreTypes.Name.Environment
import GHC.Data.UniqueSet
import GHC.Data.UniqueFM
-- Conflicts with UniqFM:
--import LazyUniqFM
import GHC.Data.FiniteMap

-- Common utilities
import GHC.Util
import GHC.Serialized
import GHC.CoreTypes.SrcLoc
import GHC.Util.Outputable
import GHC.Data.UniqueSupply
import GHC.CoreTypes.Unique           ( Unique, Uniquable(..) )
import GHC.Data.FastString
