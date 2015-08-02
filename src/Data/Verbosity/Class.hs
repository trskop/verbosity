{-# LANGUAGE NoImplicitPrelude #-}
-- |
-- Module:       $HEADER$
-- Description:  Type class for accessing Verbosity.
-- Copyright:    (c) 2015, Peter Trško
-- License:      BSD3
--
-- Maintainer:   peter.trsko@gmail.com
-- Stability:    experimental
-- Portability:  NoImplicitPrelude
--
-- Type class for accessing 'Verbosity'.
module Data.Verbosity.Class
    (
    -- * Hand Written Instance Example
    --
    -- $basicUsageExample

    -- * TemplateHaskell Example
    --
    -- $thUsageExample

    -- * HasVerbosity Type Class
      HasVerbosity(..)
    , getVerbosity
    , setVerbosity

    -- * Verbosity Re-export
    , module Data.Verbosity
    )
  where

import Control.Applicative (Const(..))
import Data.Function ((.), ($), const)
import Data.Functor (Functor)
import Data.Functor.Identity (Identity(..))

import Data.Verbosity


class HasVerbosity s where
    -- | Lens for accessing 'Verbosity'.
    verbosity :: Functor f => (Verbosity -> f Verbosity) -> s -> f s

instance HasVerbosity Verbosity where
    verbosity = ($)

-- | Specialization of 'verbosity' lens in to getter function.
getVerbosity :: HasVerbosity s => s -> Verbosity
getVerbosity = getConst . verbosity Const

-- | Specialization of 'verbosity' lens in to setter function.
setVerbosity :: HasVerbosity s => Verbosity -> s -> s
setVerbosity v = runIdentity . verbosity (const (Identity v))

-- $basicUsageExample
--
-- Lets define simple data type that looks something like:
--
-- @
-- data Config = Config
--     { _appVerbosity :: 'Verbosity'
--     , ...
--     }
--   deriving (Show, ...)
-- @
--
-- Now we can define instance of 'HasVerbosity' by hand:
--
-- @
-- instance 'HasVerbosity' Config where
--     verbosity f c@Config{_appVerbosity = a} =
--         (\b -> c{_appVerbosity = b}) 'Data.Functor.<$>' f a
-- @

-- $thUsageExample
--
-- Package <https://hackage.haskell.org/package/lens lens> has TemplateHaskell
-- functions that can define lenses for you:
--
-- @
-- import Control.Lens.TH (makeLenses)
--
-- data Config = Config
--     { _appVerbosity :: 'Verbosity'
--     , ...
--     }
--   deriving (Show, ...)
--
-- makeLenses ''Config
-- @
--
-- Don't forget to to turn on TemplateHaskell by putting following pragma at
-- the beginning of your module:
--
-- @
-- {-\# LANGUAGE TemplateHaskell \#-}
-- @
--
-- Now definition of 'HasVerbosity' instance will look like:
--
-- @
-- instance 'HasVerbosity' Config where
--     verbosity = appVerbosity
-- @