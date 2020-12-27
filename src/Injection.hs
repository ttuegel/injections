{- |
Copyright: (c) 2020 Thomas Tuegel
SPDX-License-Identifier: BSD-3-Clause
Maintainer: Thomas Tuegel <ttuegel@mailbox.org>

-}

module Injection
    ( Injection (..)
    , Retraction (..)
    ) where

import Data.Dynamic (Dynamic, Typeable, fromDynamic, toDyn)
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

{- | @Retraction@ undoes an 'Injection'.

Because 'Injection' is a lossless conversion, we can define a @Retraction@ which undoes it.
The method

> retract :: into -> Maybe from

is the (left) inverse of 'inject':

> retract (inject x) = Just x

'retract' is partial (returns 'Maybe') because the type @into@ may be larger than the type @from@;
that is, there may be values in @into@ which are not 'inject'-ed from @from@,
and in that case @retract@ may return 'Nothing'.
-}
class Injection from into => Retraction from into where
    retract :: into -> Maybe from

instance Typeable a => Injection a Dynamic where
    inject = toDyn
    {-# INLINE inject #-}

instance Typeable a => Retraction a Dynamic where
    retract = fromDynamic
    {-# INLINE retract #-}

instance Injection a (Maybe a) where
    inject = Just
    {-# INLINE inject #-}

instance Retraction a (Maybe a) where
    retract = id
    {-# INLINE retract #-}

instance Injection (Maybe a) [a] where
    inject = maybeToList
    {-# INLINE inject #-}

instance Retraction (Maybe a) [a] where
    retract [] = Just Nothing
    retract [x] = Just (Just x)
    retract _ = Nothing

instance Injection Natural Integer where
    inject = toInteger
    {-# INLINE inject #-}

instance Retraction Natural Integer where
    retract x
        | x < 0 = Nothing
        | otherwise = Just (fromInteger x)
    {-# INLINE retract #-}

instance Injection Void any where
    inject = \case {}
    {-# INLINE inject #-}
