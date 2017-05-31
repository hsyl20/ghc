{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}

-- | This module is not used by GHC itself.  Rather, it exports all of
-- the functions and types you are likely to need when writing a
-- plugin for GHC. So authors of plugins can probably get away simply
-- with saying "import GHC.Plugins".
--
-- Particularly interesting modules for plugin writers include
-- "CoreSyn" and "CoreMonad".
module GHC.Plugins
   ( module GHC.Plugins.Types
   , module GHC.Data.RdrName
   , module GHC.Data.OccName
   , module GHC.Data.Name
   , module GHC.Data.Var
   , module GHC.Data.Id
   , module GHC.Data.Id.Info
   , module CoreMonad
   , module CoreSyn
   , module GHC.Data.Literal
   , module GHC.Data.DataConstructor
   , module CoreUtils
   , module MkCore
   , module CoreFVs
   , module CoreSubst
   , module Rules
   , module GHC.Data.Annotation
   , module GHC.Config.Flags
   , module GHC.Packages
   , module GHC.Data.Module
   , module GHC.Data.Type
   , module GHC.Data.Type.Constructor
   , module GHC.Data.Coercion
   , module TysWiredIn
   , module GHC.Types
   , module GHC.Data.BasicTypes
   , module GHC.Data.Var.Set
   , module GHC.Data.Var.Environment
   , module GHC.Data.Name.Set
   , module GHC.Data.Name.Environment
   , module GHC.Data.Unique.Set
   , module GHC.Data.Unique.FiniteMap
   , module GHC.Data.FiniteMap
   , module GHC.Utils
   , module GHC.Serialized
   , module GHC.Data.SrcLoc
   , module GHC.Utils.Outputable
   , module GHC.Data.Unique.Supply
   , module GHC.Data.Unique
   , module GHC.Data.FastString
   )
where

-- Plugin stuff itself
import GHC.Plugins.Types

-- Variable naming
import GHC.Data.RdrName
import GHC.Data.OccName  hiding  ( varName {- conflicts with Var.varName -} )
import GHC.Data.Name     hiding  ( varName {- reexport from OccName, conflicts with Var.varName -} )
import GHC.Data.Var
import GHC.Data.Id       hiding  ( lazySetIdInfo, setIdExported, setIdNotExported {- all three conflict with Var -} )
import GHC.Data.Id.Info

-- Core
import CoreMonad
import CoreSyn
import GHC.Data.Literal
import GHC.Data.DataConstructor
import CoreUtils
import MkCore
import CoreFVs
import CoreSubst hiding( substTyVarBndr, substCoVarBndr, extendCvSubst )
       -- These names are also exported by Type

-- Core "extras"
import Rules
import GHC.Data.Annotation

-- Pipeline-related stuff
import GHC.Config.Flags
import GHC.Packages

-- Important GHC types
import GHC.Data.Module
import GHC.Data.Type     hiding {- conflict with CoreSubst -}
                ( substTy, extendTvSubst, extendTvSubstList, isInScope )
import GHC.Data.Coercion hiding {- conflict with CoreSubst -}
                ( substCo )
import GHC.Data.Type.Constructor
import TysWiredIn
import GHC.Types
import GHC.Data.BasicTypes hiding ( Version {- conflicts with Packages.Version -} )

-- Collections and maps
import GHC.Data.Var.Set
import GHC.Data.Var.Environment
import GHC.Data.Name.Set
import GHC.Data.Name.Environment
import GHC.Data.Unique.Set
import GHC.Data.Unique.FiniteMap
-- Conflicts with UniqFM:
--import LazyUniqFM
import GHC.Data.FiniteMap

-- Common utilities
import GHC.Utils
import GHC.Serialized
import GHC.Data.SrcLoc
import GHC.Utils.Outputable
import GHC.Data.Unique.Supply
import GHC.Data.Unique           ( Unique, Uniquable(..) )
import GHC.Data.FastString
