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
import Data.Fixed (Fixed, HasResolution)
import Data.Functor.Const (Const (..))
import Data.Maybe (maybeToList)
import Data.Ratio (Ratio)
import qualified Data.Ratio as Ratio
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Lazy as Lazy (Text)
import qualified Data.Text.Lazy as Text.Lazy
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

Although it is often possible to infer the type parameters of this class,
it is advisable to specify one or both of the parameters to @inject@
using a type signature or the @TypeApplications@ language extension.
Specifying the type parameters will give clearer error messages from the type checker in any case.

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

Although it is often possible to infer the type parameters of this class,
it is advisable to specify one or both of the parameters to @retract@
using a type signature or the @TypeApplications@ language extension.
Specifying the type parameters will give clearer error messages from the type checker in any case.

-}
class Injection from into => Retraction from into where
    retract :: into -> Maybe from

instance {-# OVERLAPPABLE #-} Injection a a where
    inject = id
    {-# INLINE inject #-}

instance {-# OVERLAPPABLE #-} Retraction a a where
    retract = Just
    {-# INLINE retract #-}

instance Typeable a => Injection a Dynamic where
    inject = toDyn
    {-# INLINE inject #-}

instance Typeable a => Retraction a Dynamic where
    retract = fromDynamic
    {-# INLINE retract #-}

instance Injection a b => Injection a (Maybe b) where
    inject = Just . inject
    {-# INLINE inject #-}

instance Retraction a b => Retraction a (Maybe b) where
    retract = \x -> x >>= retract @a @b
    {-# INLINE retract #-}

instance Injection a b => Injection (Maybe a) [b] where
    inject = maybeToList . fmap (inject @a @b)
    {-# INLINE inject #-}

instance Retraction a b => Retraction (Maybe a) [b] where
    retract [] = Just Nothing
    retract [b] = Just <$> retract @a @b b
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

instance Injection String Text where
    inject = Text.pack
    {-# INLINE inject #-}

instance Injection Text String where
    inject = Text.unpack
    {-# INLINE inject #-}

instance Injection String Lazy.Text where
    inject = Text.Lazy.pack
    {-# INLINE inject #-}

instance Injection Lazy.Text String where
    inject = Text.Lazy.unpack
    {-# INLINE inject #-}

instance Injection Text Lazy.Text where
    inject = Text.Lazy.fromStrict
    {-# INLINE inject #-}

instance Injection Lazy.Text Text where
    inject = Text.Lazy.toStrict
    {-# INLINE inject #-}

instance HasResolution a => Injection Integer (Fixed a) where
    inject = fromInteger
    {-# INLINE inject #-}

instance HasResolution a => Retraction Integer (Fixed a) where
    retract x = retract @Integer (toRational x)
    {-# INLINE retract #-}

instance Injection a (Const a b) where
    inject = Const
    {-# INLINE inject #-}

instance Injection (Const a b) a where
    inject = getConst
    {-# INLINE inject #-}

instance Injection Integer (Ratio Integer) where
    inject = fromInteger
    {-# INLINE inject #-}

instance Retraction Integer (Ratio Integer) where
    retract x
        | Ratio.denominator x == 1 = Just (Ratio.numerator x)
        | otherwise = Nothing
    {-# INLINE retract #-}
