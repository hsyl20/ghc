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
   , module GHC.Types.RdrName
   , module GHC.Types.OccName
   , module GHC.Types.Name
   , module GHC.Types.Var
   , module GHC.Types.Id
   , module GHC.Types.Id.Info
   , module GHC.Core.Monad
   , module GHC.Core.Syntax
   , module GHC.Types.Literal
   , module GHC.Types.DataCon
   , module GHC.Core.Utils
   , module GHC.Core.Syntax.Make
   , module GHC.Core.FreeVars
   , module GHC.Core.Substitution
   , module GHC.Core.Rules
   , module GHC.Types.Annotation
   , module GHC.Config.Flags
   , module GHC.Packages
   , module GHC.Types.Module
   , module GHC.Types.Type
   , module GHC.Types.TyCon
   , module GHC.Types.Coercion
   , module GHC.Builtin.Types
   , module GHC.Types.Base
   , module GHC.Types.BasicTypes
   , module GHC.Types.Var.Set
   , module GHC.Types.Var.Environment
   , module GHC.Types.Name.Set
   , module GHC.Types.Name.Environment
   , module GHC.Data.UniqueSet
   , module GHC.Data.UniqueFM
   , module GHC.Data.FiniteMap
   , module GHC.Utils
   , module GHC.Serialized
   , module GHC.Types.SrcLoc
   , module GHC.Utils.Outputable
   , module GHC.Data.UniqueSupply
   , module GHC.Types.Unique
   , module GHC.Data.FastString
   )
where

-- Plugin stuff itself
import GHC.Plugin.Types

-- Variable naming
import GHC.Types.RdrName
import GHC.Types.OccName  hiding  ( varName {- conflicts with Var.varName -} )
import GHC.Types.Name     hiding  ( varName {- reexport from OccName,
                                                conflicts with Var.varName -} )
import GHC.Types.Var
import GHC.Types.Id       hiding
         {- all three conflict with Var -}
         ( lazySetIdInfo, setIdExported, setIdNotExported )
import GHC.Types.Id.Info

-- Core
import GHC.Core.Monad
import GHC.Core.Syntax
import GHC.Types.Literal
import GHC.Types.DataCon
import GHC.Core.Utils
import GHC.Core.Syntax.Make
import GHC.Core.FreeVars
import GHC.Core.Substitution hiding
         -- These names are also exported by Type
         ( substTyVarBndr, substCoVarBndr, extendCvSubst )

-- Core "extras"
import GHC.Core.Rules
import GHC.Types.Annotation

-- Pipeline-related stuff
import GHC.Config.Flags
import GHC.Packages

-- Important GHC types
import GHC.Types.Module
import GHC.Types.Type     hiding
                {- conflict with GHC.Core.Substitution -}
                ( substTy, extendTvSubst, extendTvSubstList, isInScope )
import GHC.Types.Coercion hiding
                {- conflict with GHC.Core.Substitution -}
                ( substCo )
import GHC.Types.TyCon
import GHC.Builtin.Types
import GHC.Types.Base
import GHC.Types.BasicTypes hiding
               {- conflicts with GHC.Packages.Version -}
               ( Version )

-- Collections and maps
import GHC.Types.Var.Set
import GHC.Types.Var.Environment
import GHC.Types.Name.Set
import GHC.Types.Name.Environment
import GHC.Data.UniqueSet
import GHC.Data.UniqueFM
-- Conflicts with UniqFM:
--import LazyUniqFM
import GHC.Data.FiniteMap

-- Common utilities
import GHC.Utils
import GHC.Serialized
import GHC.Types.SrcLoc
import GHC.Utils.Outputable
import GHC.Data.UniqueSupply
import GHC.Types.Unique           ( Unique, Uniquable(..) )
import GHC.Data.FastString
