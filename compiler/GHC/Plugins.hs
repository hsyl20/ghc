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
   , module RdrName
   , module OccName
   , module Name
   , module Var
   , module Id
   , module IdInfo
   , module CoreMonad
   , module CoreSyn
   , module Literal
   , module DataCon
   , module CoreUtils
   , module MkCore
   , module CoreFVs
   , module CoreSubst
   , module Rules
   , module GHC.Types.Annotations
   , module GHC.Config.Flags
   , module GHC.Packages
   , module Module
   , module Type
   , module TyCon
   , module Coercion
   , module TysWiredIn
   , module GHC.Types
   , module BasicTypes
   , module VarSet
   , module VarEnv
   , module NameSet
   , module NameEnv
   , module UniqSet
   , module GHC.Utils.Data.Unique.FiniteMap
   , module GHC.Utils.Data.FiniteMap
   , module Util
   , module GHC.Serialized
   , module SrcLoc
   , module Outputable
   , module UniqSupply
   , module Unique
   , module FastString
   )
where

-- Plugin stuff itself
import GHC.Plugins.Types

-- Variable naming
import RdrName
import OccName  hiding  ( varName {- conflicts with Var.varName -} )
import Name     hiding  ( varName {- reexport from OccName, conflicts with Var.varName -} )
import Var
import Id       hiding  ( lazySetIdInfo, setIdExported, setIdNotExported {- all three conflict with Var -} )
import IdInfo

-- Core
import CoreMonad
import CoreSyn
import Literal
import DataCon
import CoreUtils
import MkCore
import CoreFVs
import CoreSubst hiding( substTyVarBndr, substCoVarBndr, extendCvSubst )
       -- These names are also exported by Type

-- Core "extras"
import Rules
import GHC.Types.Annotations

-- Pipeline-related stuff
import GHC.Config.Flags
import GHC.Packages

-- Important GHC types
import Module
import Type     hiding {- conflict with CoreSubst -}
                ( substTy, extendTvSubst, extendTvSubstList, isInScope )
import Coercion hiding {- conflict with CoreSubst -}
                ( substCo )
import TyCon
import TysWiredIn
import GHC.Types
import BasicTypes hiding ( Version {- conflicts with Packages.Version -} )

-- Collections and maps
import VarSet
import VarEnv
import NameSet
import NameEnv
import UniqSet
import GHC.Utils.Data.Unique.FiniteMap
-- Conflicts with UniqFM:
--import LazyUniqFM
import GHC.Utils.Data.FiniteMap

-- Common utilities
import Util
import GHC.Serialized
import SrcLoc
import Outputable
import UniqSupply
import Unique           ( Unique, Uniquable(..) )
import FastString
