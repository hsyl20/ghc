{-
(c) The GRASP/AQUA Project, Glasgow University, 1992-1998

-}

{-# LANGUAGE CPP #-}

-- | The @GHC.Builtin.Utils@ interface to the compiler's prelude knowledge.
--
-- This module serves as the central gathering point for names which the
-- compiler knows something about. This includes functions for,
--
--  * discerning whether a 'Name' is known-key
--
--  * given a 'Unique', looking up its corresponding known-key 'Name'
--
-- See Note [Known-key names] and Note [About wired-in things] for information
-- about the two types of prelude things in GHC.
--
module GHC.Builtin.Utils (
        -- * Known-key names
        isKnownKeyName,
        lookupKnownKeyName,

        -- ** Internal use
        -- | 'knownKeyNames' is exported to seed the original name cache only;
        -- if you find yourself wanting to look at it you might consider using
        -- 'lookupKnownKeyName' or 'isKnownKeyName'.
        knownKeyNames,

        -- * Miscellaneous
        wiredInIds, ghcPrimIds,
        primOpRules, builtinRules,

        ghcPrimExports,
        primOpId,

        -- * Random other things
        maybeCharLikeCon, maybeIntLikeCon,

        -- * Class categories
        isNumericClass, isStandardClass

    ) where

#include "HsVersions.h"

import GHC.Builtin.Uniques
import GHC.Data.Unique           ( isValidKnownKeyUnique )

import GHC.Data.ConstructorLike          ( ConLike(..) )
import GHC.Builtin.Names.TemplateHaskell          ( templateHaskellNames )
import GHC.Builtin.Names
import GHC.IR.Core.Transform.ConstantFolding
import GHC.Data.Available
import GHC.Builtin.Primitive.Operation
import GHC.Data.DataConstructor
import GHC.Data.Id
import GHC.Data.Name
import GHC.Data.Name.Environment
import GHC.Data.Id.Make
import GHC.Builtin.Primitive.Type
import GHC.Builtin.Type
import GHC.Types
import GHC.Data.Class
import GHC.Data.Type.Constructor
import GHC.Data.Unique.FiniteMap
import GHC.Utils
import GHC.Utils.Panic
import {-# SOURCE #-} GHC.Builtin.TypeNats ( typeNatTyCons )

import Control.Applicative ((<|>))
import Data.List        ( intercalate )
import Data.Array
import Data.Maybe

{-
************************************************************************
*                                                                      *
\subsection[builtinNameInfo]{Lookup built-in names}
*                                                                      *
************************************************************************

Note [About wired-in things]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~
* Wired-in things are Ids\/TyCons that are completely known to the compiler.
  They are global values in GHC, (e.g.  listTyCon :: TyCon).

* A wired in Name contains the thing itself inside the Name:
        see Name.wiredInNameTyThing_maybe
  (E.g. listTyConName contains listTyCon.

* The name cache is initialised with (the names of) all wired-in things
  (except tuples and sums; see Note [Known-])

* The type environment itself contains no wired in things. The type
  checker sees if the Name is wired in before looking up the name in
  the type environment.

* GHC.IR.Interface.Utils prunes out wired-in things before putting them in an interface file.
  So interface files never contain wired-in things.
-}


-- | This list is used to ensure that when you say "Prelude.map" in your source
-- code, or in an interface file, you get a Name with the correct known key (See
-- Note [Known-key names] in GHC.Builtin.Names)
knownKeyNames :: [Name]
knownKeyNames
  | debugIsOn
  , Just badNamesStr <- knownKeyNamesOkay all_names
  = panic ("badAllKnownKeyNames:\n" ++ badNamesStr)
       -- NB: We can't use ppr here, because this is sometimes evaluated in a
       -- context where there are no DynFlags available, leading to a cryptic
       -- "<<details unavailable>>" error. (This seems to happen only in the
       -- stage 2 compiler, for reasons I [Richard] have no clue of.)
  | otherwise
  = all_names
  where
    all_names =
      concat [ wired_tycon_kk_names funTyCon
             , concatMap wired_tycon_kk_names primTyCons

             , concatMap wired_tycon_kk_names wiredInTyCons
               -- Does not include tuples

             , concatMap wired_tycon_kk_names typeNatTyCons

             , map idName wiredInIds
             , map (idName . primOpId) allThePrimOps
             , basicKnownKeyNames
             , templateHaskellNames
             ]
    -- All of the names associated with a wired-in TyCon.
    -- This includes the TyCon itself, its DataCons and promoted TyCons.
    wired_tycon_kk_names :: TyCon -> [Name]
    wired_tycon_kk_names tc =
        tyConName tc : (rep_names tc ++ implicits)
      where implicits = concatMap thing_kk_names (implicitTyConThings tc)

    wired_datacon_kk_names :: DataCon -> [Name]
    wired_datacon_kk_names dc =
      dataConName dc : rep_names (promoteDataCon dc)

    thing_kk_names :: TyThing -> [Name]
    thing_kk_names (ATyCon tc)                 = wired_tycon_kk_names tc
    thing_kk_names (AConLike (RealDataCon dc)) = wired_datacon_kk_names dc
    thing_kk_names thing                       = [getName thing]

    -- The TyConRepName for a known-key TyCon has a known key,
    -- but isn't itself an implicit thing.  Yurgh.
    -- NB: if any of the wired-in TyCons had record fields, the record
    --     field names would be in a similar situation.  Ditto class ops.
    --     But it happens that there aren't any
    rep_names tc = case tyConRepName_maybe tc of
                        Just n  -> [n]
                        Nothing -> []

-- | Check the known-key names list of consistency.
knownKeyNamesOkay :: [Name] -> Maybe String
knownKeyNamesOkay all_names
  | ns@(_:_) <- filter (not . isValidKnownKeyUnique . getUnique) all_names
  = Just $ "    Out-of-range known-key uniques: ["
        ++ intercalate ", " (map (occNameString . nameOccName) ns) ++
         "]"
  | null badNamesPairs
  = Nothing
  | otherwise
  = Just badNamesStr
  where
    namesEnv      = foldl (\m n -> extendNameEnv_Acc (:) singleton m n n)
                          emptyUFM all_names
    badNamesEnv   = filterNameEnv (\ns -> length ns > 1) namesEnv
    badNamesPairs = nonDetUFMToList badNamesEnv
      -- It's OK to use nonDetUFMToList here because the ordering only affects
      -- the message when we get a panic
    badNamesStrs  = map pairToStr badNamesPairs
    badNamesStr   = unlines badNamesStrs

    pairToStr (uniq, ns) = "        " ++
                           show uniq ++
                           ": [" ++
                           intercalate ", " (map (occNameString . nameOccName) ns) ++
                           "]"

-- | Given a 'Unique' lookup its associated 'Name' if it corresponds to a
-- known-key thing.
lookupKnownKeyName :: Unique -> Maybe Name
lookupKnownKeyName u =
    knownUniqueName u <|> lookupUFM knownKeysMap u

-- | Is a 'Name' known-key?
isKnownKeyName :: Name -> Bool
isKnownKeyName n =
    isJust (knownUniqueName $ nameUnique n) || elemUFM n knownKeysMap

knownKeysMap :: UniqFM Name
knownKeysMap = listToUFM [ (nameUnique n, n) | n <- knownKeyNames ]

{-
We let a lot of "non-standard" values be visible, so that we can make
sense of them in interface pragmas. It's cool, though they all have
"non-standard" names, so they won't get past the parser in user code.

************************************************************************
*                                                                      *
                PrimOpIds
*                                                                      *
************************************************************************
-}

primOpIds :: Array Int Id
-- A cache of the PrimOp Ids, indexed by PrimOp tag
primOpIds = array (1,maxPrimOpTag) [ (primOpTag op, mkPrimOpId op)
                                   | op <- allThePrimOps ]

primOpId :: PrimOp -> Id
primOpId op = primOpIds ! primOpTag op

{-
************************************************************************
*                                                                      *
            Export lists for pseudo-modules (GHC.Prim)
*                                                                      *
************************************************************************

GHC.Prim "exports" all the primops and primitive types, some
wired-in Ids.
-}

ghcPrimExports :: [IfaceExport]
ghcPrimExports
 = map (avail . idName) ghcPrimIds ++
   map (avail . idName . primOpId) allThePrimOps ++
   [ AvailTC n [n] []
   | tc <- funTyCon : primTyCons, let n = tyConName tc  ]

{-
************************************************************************
*                                                                      *
            Built-in keys
*                                                                      *
************************************************************************

ToDo: make it do the ``like'' part properly (as in 0.26 and before).
-}

maybeCharLikeCon, maybeIntLikeCon :: DataCon -> Bool
maybeCharLikeCon con = con `hasKey` charDataConKey
maybeIntLikeCon  con = con `hasKey` intDataConKey

{-
************************************************************************
*                                                                      *
            Class predicates
*                                                                      *
************************************************************************
-}

isNumericClass, isStandardClass :: Class -> Bool

isNumericClass     clas = classKey clas `is_elem` numericClassKeys
isStandardClass    clas = classKey clas `is_elem` standardClassKeys

is_elem :: Eq a => a -> [a] -> Bool
is_elem = isIn "is_X_Class"
