{-# LANGUAGE CPP #-}
{-# LANGUAGE NoImplicitPrelude #-}

#ifdef DERIVE_DATA_TYPEABLE
{-# LANGUAGE DeriveDataTypeable #-}
#endif

#ifdef DERIVE_GHC_GENERICS
{-# LANGUAGE DeriveGeneric #-}
#endif

#ifdef DECLARE_NFDATA_INSTANCE
{-# LANGUAGE BangPatterns #-}
#endif

-- |
-- Module:       $HEADER$
-- Description:  Verbosity enum.
-- Copyright:    (c) 2015-2016 Peter Trško
-- License:      BSD3
--
-- Maintainer:   peter.trsko@gmail.com
-- Stability:    experimental
-- Portability:  BangPatterns (optional), CPP, NoImplicitPrelude,
--               DeriveDataTypeable (optional), DeriveGeneric (optional)
--
-- Simple enum that encodes application 'Verbosity'.
module Data.Verbosity
    ( Verbosity(..)
    , increment
    , increment'
    , fromInt
#ifdef DERIVE_DATA_TYPEABLE
    , parse
#endif
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
import Data.Eq (Eq)
import Data.Int (Int)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Ord (Ord(..))
import Text.Read (Read)
import Text.Show (Show)

#ifdef DERIVE_DATA_TYPEABLE
import Data.Data (Data(toConstr), Typeable, showConstr)
import Data.List (lookup)
import Data.String (IsString(fromString))
#endif

#ifdef DERIVE_GHC_GENERICS
import GHC.Generics (Generic)
#endif

#if defined(DECLARE_BINARY_INSTANCE) || defined(DECLARE_SERIALIZE_INSTANCE)
import Control.Applicative ((<$>))
import Data.Function ((.))
#endif

#ifdef DECLARE_BINARY_INSTANCE
import Data.Binary (Binary(get, put))
import qualified Data.Binary as Binary (getWord8, putWord8)
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
  deriving
    ( Bounded, Enum, Eq, Ord, Read, Show
#ifdef DERIVE_GHC_GENERICS
    , Generic
#endif
#ifdef DERIVE_DATA_TYPEABLE
    , Data, Typeable
#endif
    )

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

#ifdef DECLARE_NFDATA_INSTANCE
instance NFData Verbosity where
    rnf !_ = ()
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

#ifdef DERIVE_DATA_TYPEABLE
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
    str = fromString . showConstr . toConstr
#endif
