{- |
Copyright: (c) 2020 Thomas Tuegel
SPDX-License-Identifier: BSD-3-Clause
Maintainer: Thomas Tuegel <ttuegel@mailbox.org>

-}

module Projection
    ( Projection (..)
    , Section (..)
    ) where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

{- | @Projection@ describes a lossless conversion that extracts (projects out) one type from another.

The sole method of this class,

> project :: whole -> part

takes a value @input :: whole@ and extracts a value @output :: part@ which was included entirely in the @input@.
@project@ must /cover/ @part@, that is, every value in @part@ must the output of @project@ for some value in @whole@.
In mathematical terminology, @project@ is /surjective/:

> forall y. exists x. project x = y

We might also say that it /projects/ @input :: whole@ onto its equivalent class;
the typeclass gets its name from this terminology.

@Projection@ models the "has-a" relationship used in languages with subtypes (such as in object-oriented programming),
but an explicit cast with @project@ is required in Haskell.

Although it is often possible to infer the type parameters of this class,
it is advisable to specify one or both of the parameters to @project@
using a type signature or the @TypeApplications@ language extension.
Specifying the type parameters will give clearer error messages from the type checker in any case.

-}
class Projection whole part where
    project :: whole -> part

{- | @Section@ undoes a 'Projection'.

The method

> update :: whole -> part -> whole

is the (right) inverse of 'project':

> project (update x y) = y

In mathematics, /section/ refers generically to the right inverse of a morphism.
Keeping with Haskell tradition, we are borrowing this generic term and using it in a very specific way:
@Section@ is the right inverse of @Projection@ in particular.

Although it is often possible to infer the type parameters of this class,
it is advisable to specify one or both of the parameters to @retract@
using a type signature or the @TypeApplications@ language extension.
Specifying the type parameters will give clearer error messages from the type checker in any case.

-}
class Projection whole part => Section whole part where
    update :: whole -> part -> whole

instance Projection a a where
    project = id
    {-# INLINE project #-}

instance Section a a where
    update _ = id
    {-# INLINE update #-}

instance Ord key => Projection [(key, value)] (Map key value) where
    project = Map.fromList
    {-# INLINE project #-}

instance Ord key => Projection [key] (Set key) where
    project = Set.fromList
    {-# INLINE project #-}
