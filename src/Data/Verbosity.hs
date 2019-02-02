{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeOperators #-}

#ifdef DECLARE_NFDATA_INSTANCE
{-# LANGUAGE BangPatterns #-}
#endif

#ifdef DECLARE_SAFECOPY_INSTANCE
{-# LANGUAGE TemplateHaskell #-}
#endif

#ifdef DECLARE_DHALL_INSTANCES
{-# LANGUAGE DeriveAnyClass #-}
#endif

-- |
-- Module:       $HEADER$
-- Description:  Verbosity enum.
-- Copyright:    (c) 2015-2019 Peter Trško
-- License:      BSD3
--
-- Maintainer:   peter.trsko@gmail.com
-- Stability:    experimental
-- Portability:  GHC specific language extensions.
--
-- Simple enum that encodes application 'Verbosity'.
module Data.Verbosity
    ( Verbosity(..)
    , increment
    , increment'
    , fromInt
    , parse
    )
  where

import Prelude
    ( Bounded(maxBound, minBound)
    , Enum(fromEnum, succ, toEnum)
#ifdef DECLARE_BINARY_INSTANCE
    , fromIntegral
#endif
    )

import Data.Bool ((&&), otherwise)
import Data.Data (Data)
import Data.Eq (Eq)
import Data.Int (Int)
import Data.Kind (Type)
import Data.List (lookup)
import Data.Maybe (Maybe(Just, Nothing), fromMaybe)
import Data.Ord
    ( Ord
    , (<)
    , (<=)
    , (>=)
#ifdef DECLARE_LATTICE_INSTANCES
    , max
    , min
#endif
    )
import Data.String (IsString, String, fromString)
import GHC.Generics
    ( (:+:)(L1, R1)
    , C1
    , Constructor
    , D1
    , Generic
    , M1(M1)
    , conName
    , from
    )
import Text.Read (Read)
import Text.Show (Show)

#if defined(DECLARE_BINARY_INSTANCE) || defined(DECLARE_SERIALIZE_INSTANCE)
import Control.Applicative ((<$>))
import Data.Function ((.))
#endif

#ifdef DECLARE_BINARY_INSTANCE
import Data.Binary (Binary(get, put))
import qualified Data.Binary as Binary (getWord8, putWord8)
#endif

#ifdef DECLARE_SAFECOPY_INSTANCE
import Data.SafeCopy (deriveSafeCopy)
import qualified Data.SafeCopy as SafeCopy (base)
#endif

#ifdef DECLARE_SERIALIZE_INSTANCE
import qualified Data.Serialize as Cereal (Serialize(..), getWord8, putWord8)
#endif

#ifdef DECLARE_DEFAULT_INSTANCE
import Data.Default.Class (Default(def))
#endif

#ifdef DECLARE_NFDATA_INSTANCE
import Control.DeepSeq (NFData(rnf))
#endif

#ifdef DECLARE_LATTICE_INSTANCES
import Algebra.Lattice
    ( BoundedJoinSemiLattice(bottom)
    , BoundedLattice
    , BoundedMeetSemiLattice(top)
    , JoinSemiLattice((\/))
    , Lattice
    , MeetSemiLattice((/\))
    )
#endif

#ifdef DECLARE_DHALL_INSTANCES
import qualified Dhall (Inject, Interpret)
#endif

-- | Ordering:
--
-- @
-- 'Silent' < 'Normal' < 'Verbose' < 'Annoying'
-- @
--
-- Bounds:
--
-- @
-- 'minBound' = 'Silent'; 'maxBound' = 'Annoying'
-- @
--
-- Enum:
--
-- @
-- map 'fromEnum' ['Silent' .. 'Annoying'] = [0, 1, 2, 3]
-- @
data Verbosity
    = Silent
    -- ^ Don't print any messages.
    | Normal
    -- ^ Print only important messages. (default)
    | Verbose
    -- ^ Print anything that comes in to mind.
    | Annoying
    -- ^ Print debugging/tracing information.
  deriving stock
    ( Bounded
    , Data
    , Enum
    , Eq
    , Generic
    , Ord
    , Read
    , Show
    )
#ifdef DECLARE_DHALL_INSTANCES
  deriving anyclass (Dhall.Inject, Dhall.Interpret)
#endif

#ifdef DECLARE_DEFAULT_INSTANCE
-- | @'def' = 'Normal'@
instance Default Verbosity where
    def = Normal
#endif

#ifdef DECLARE_BINARY_INSTANCE
-- | Encoded as one byte in range @['minBound' .. 'maxBound' :: Verbosity]@.
instance Binary Verbosity where
    get = toEnum . fromIntegral <$> Binary.getWord8
    put = Binary.putWord8 . fromIntegral . fromEnum
#endif

#ifdef DECLARE_SERIALIZE_INSTANCE
-- | Encoded as one byte in range @['minBound' .. 'maxBound' :: Verbosity]@.
instance Cereal.Serialize Verbosity where
    get = toEnum . fromIntegral <$> Cereal.getWord8
    put = Cereal.putWord8 . fromIntegral . fromEnum
#endif

#ifdef DECLARE_SAFECOPY_INSTANCE
deriveSafeCopy 0 'SafeCopy.base ''Verbosity
#endif

#ifdef DECLARE_NFDATA_INSTANCE
instance NFData Verbosity where
    rnf !_ = ()
#endif

#ifdef DECLARE_LATTICE_INSTANCES
-- | @('\/') = 'max'@
instance JoinSemiLattice Verbosity where
    (\/) = max

-- | @'bottom' = 'Silent'@
instance BoundedJoinSemiLattice Verbosity where
    bottom = minBound

-- | @('/\') = 'min'@
instance MeetSemiLattice Verbosity where
    (/\) = min

-- | @'top' = 'Annoying'@
instance BoundedMeetSemiLattice Verbosity where
    top = maxBound

instance Lattice Verbosity
instance BoundedLattice Verbosity
#endif

-- | Increment verbosity level. Return 'Nothing' if trying to icrement beyond
-- 'maxBound'.
increment :: Verbosity -> Maybe Verbosity
increment v
  | v < maxBound = Just (succ v)
  | otherwise    = Nothing

-- | Variant of 'increment' that doesn't fail when 'maxBound' is reached. It
-- is defined as:
--
-- @
-- 'increment'' v = 'fromMaybe' v ('increment' v)
-- @
increment' :: Verbosity -> Verbosity
increment' v = fromMaybe v (increment v)

-- | Safe version of 'toEnum' specialized to 'Verbosity'.
fromInt :: Int -> Maybe Verbosity
fromInt n
  | n >= minVerbosity && n <= maxVerbosity = Just (toEnum n)
  | otherwise                              = Nothing
  where
    -- This makes code robust enough to survive changes in Verbosity
    -- definition.
    minVerbosity = fromEnum (minBound :: Verbosity)
    maxVerbosity = fromEnum (maxBound :: Verbosity)

-- | Generic 'Verbosity' parsing function.
--
-- Use <https://hackage.haskell.org/package/case-insensitive case-insensitive>
-- package to make this function case insensitive:
--
-- @
-- ghci> import Data.Verbosity as Verbosity
-- ghci> import qualified Data.CaseInsensitive as CI (mk)
-- ghci> Verbosity.parse (CI.mk "silent")
-- Just Silent
-- @
parse :: (Eq string, IsString string) => string -> Maybe Verbosity
parse = (`lookup` [(str v, v) | v <- [minBound..maxBound :: Verbosity]])
  where
    str = fromString . gDataConstructorName . from

class HasDataConstructors (f :: Type -> Type) where
    gDataConstructorName :: f x -> String

instance HasDataConstructors f => HasDataConstructors (D1 c f) where
    gDataConstructorName (M1 x) = gDataConstructorName x

instance
    ( HasDataConstructors x
    , HasDataConstructors y
    )
    => HasDataConstructors (x :+: y)
  where
    gDataConstructorName = \case
        L1 l -> gDataConstructorName l
        R1 r -> gDataConstructorName r

instance Constructor c => HasDataConstructors (C1 c f) where
    gDataConstructorName = conName
