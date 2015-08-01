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
-- Enum that encodes 'Verbosity'.
module Data.Verbosity
  where

import Prelude
    ( Bounded
#ifdef DECLARE_BINARY_INSTANCE
    , Enum(fromEnum, toEnum)
    , fromIntegral
#else
    , Enum
#endif
    )

import Data.Data (Data, Typeable)
import Data.Eq (Eq)
import Data.Ord (Ord)
import GHC.Generics (Generic)
import Text.Show (Show)
import Text.Read (Read)

#ifdef DECLARE_BINARY_INSTANCE
import Control.Applicative ((<$>))
import Data.Function ((.))

import Data.Binary (Binary(get, put), getWord8, putWord8)
#endif

#ifdef DECLARE_DEFAULT_INSTANCE
import Data.Default.Class (Default(def))
#endif


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
-- Encoded as byte in range
-- ['fromEnum' v | v <- ['Prelude.minBound'..'Prelude.maxBound' :: 'Verbosity']
instance Binary Verbosity where
    get = toEnum . fromIntegral <$> getWord8
    put = putWord8 . fromIntegral . fromEnum
#endif
