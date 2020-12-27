{- |
Copyright: (c) 2020 Thomas Tuegel
SPDX-License-Identifier: BSD-3-Clause
Maintainer: Thomas Tuegel <ttuegel@mailbox.org>

-}

module Injection
    ( Injection (..)
    ) where

import Data.Dynamic (Dynamic, Typeable, toDyn)
import Data.Maybe (maybeToList)
import Data.Void (Void)
import Numeric.Natural (Natural)

{- | @Injection@ describes a lossless conversion from one type to another.

The sole method of this class,

> inject :: from -> into

takes a value @input :: from@ and returns a value @output :: into@ which preserves all the information contained in the input.
Specifically, each @input@ is mapped to a /unique/ @output@.
In mathematical terminology, @inject@ is /injective/:

> inject a ≡ inject b → a ≡ b

The name of the class is derived from the mathematical term.

@Injection@ models the "is-a" relationship used in languages with subtypes (such as in object-oriented programming),
but an explicit cast with @inject@ is required in Haskell.

-}
class Injection from into where
    inject :: from -> into

instance Typeable a => Injection a Dynamic where
    inject = toDyn
    {-# INLINE inject #-}

instance Injection a (Maybe a) where
    inject = Just
    {-# INLINE inject #-}

instance Injection (Maybe a) [a] where
    inject = maybeToList
    {-# INLINE inject #-}

instance Injection Natural Integer where
    inject = toInteger
    {-# INLINE inject #-}

instance Injection Void any where
    inject = \case {}
    {-# INLINE inject #-}
