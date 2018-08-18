{-# LANGUAGE DeriveGeneric #-}
-- |
-- Module:       $HEADER$
-- Description:  Example of hand written HasVerbosity instance.
-- Copyright:    (c) 2015, Peter Trško
-- License:      BSD3
--
-- Maintainer:   peter.trsko@gmail.com
--
-- Example of hand written HasVerbosity instance.
module Example.Config
    ( Config
    , module Data.Verbosity.Class
    )
  where

import GHC.Generics (Generic)

import Data.Generics.Product.Typed (HasType(typed))

import Data.Verbosity.Class


data Config = Config
    { _appVerbosity :: Verbosity
    }
  deriving (Generic, Show)

instance HasVerbosity Config where
    verbosity = typed
