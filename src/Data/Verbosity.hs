{-# LANGUAGE CPP #-}
{-# LANGUAGE NoImplicitPrelude #-}

#ifdef DERIVE_DATA_TYPEABLE
{-# LANGUAGE DeriveDataTypeable #-}
#endif

#ifdef DERIVE_GHC_GENERICS
{-# LANGUAGE DeriveGeneric #-}
#endif

-- |
-- Module:       $HEADER$
-- Description:  Verbosity enum.
-- Copyright:    (c) 2015, Peter Trško
-- License:      BSD3
--
-- Maintainer:   peter.trsko@gmail.com
-- Stability:    experimental
-- Portability:  CPP, NoImplicitPrelude, DeriveDataTypeable (optional),
--               DeriveGeneric (optional)
--
-- Simple enum that encodes application 'Verbosity'.
module Data.Verbosity
    (
    -- * Verbosity
      Verbosity(..)
    , fromInt
    )
  where

import Prelude
    ( Bounded(maxBound, minBound)
    , Enum(fromEnum, toEnum)
#ifdef DECLARE_BINARY_INSTANCE
    , fromIntegral
#endif
    )

import Data.Bool ((&&), otherwise)
import Data.Eq (Eq)
import Data.Int (Int)
import Data.Maybe (Maybe(..))
import Data.Ord (Ord(..))
import Text.Read (Read)
import Text.Show (Show)

#ifdef DERIVE_DATA_TYPEABLE
import Data.Data (Data, Typeable)
#endif

#ifdef DERIVE_GHC_GENERICS
import GHC.Generics (Generic)
#endif

#ifdef DECLARE_BINARY_INSTANCE
import Control.Applicative ((<$>))
import Data.Function ((.))

import Data.Binary (Binary(get, put), getWord8, putWord8)
#endif

#ifdef DECLARE_DEFAULT_INSTANCE
import Data.Default.Class (Default(def))
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
    ( Bounded
    , Enum
    , Eq
    , Ord
    , Read
    , Show
#ifdef DERIVE_GHC_GENERICS
    , Generic
#endif
#ifdef DERIVE_DATA_TYPEABLE
    , Data
    , Typeable
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
    get = toEnum . fromIntegral <$> getWord8
    put = putWord8 . fromIntegral . fromEnum
#endif

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
