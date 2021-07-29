# injection

[![GitHub CI](https://github.com/ttuegel/injection/workflows/CI/badge.svg)](https://github.com/ttuegel/injection/actions)
[![BSD-3-Clause license](https://img.shields.io/badge/license-BSD--3--Clause-blue.svg)](LICENSE)

Description: A library describing canonical injections and projections between Haskell types.

## Injection

The class `Injection` describes a lossless conversion from one type to another;
that is, the sole method of the class,

```.hs
inject :: from -> into
```

takes a value `input :: from` and returns a value `output :: into` which preserves all the information contained in the input.
Specifically, each `input` is mapped to a _unique_ `output`.
In mathematical terminology, `inject` is _injective_:

```.hs
inject a ≡ inject b → a ≡ b
```

the outputs of two calls to `inject` is the same only if the inputs are the same.
The name of the class is derived from the mathematical term.

`Injection` models the "is-a" relationship used in languages with subtypes (such as in object-oriented programming),
but an explicit cast with `inject` is required in Haskell.

### Examples

There are many examples of injections scattered throughout Haskell,
but the `Injection` class collects them in one place.
Here we present some examples.
Some of these instances have been generalized in the library,
but we present the simplified version here for explanatory purposes.

`Dynamic` is a dynamically-typed wrapper for values of all `Typeable` types.
Wrapping a value with `toDyn` preserves the value exactly, so we know that it is injective:

```.hs
instance Typeable a => Injection a Dynamic where
    inject = toDyn
```

The constructor `Just :: a -> Maybe a` is an injective function:

```.hs
instance Injection a (Maybe a) where
    inject = Just
```

Constructors with a single argument are always injective functions.
We must use `Just` here; if we had written `inject _ = Nothing`, that would violate the injective law.
Usually, any putative definition of `inject` with a wildcard match on the left-hand side will fail to be injective.
There are exceptions to this guideline; for example, this instance is injective:

```.hs
-- BAD!
instance Injection () (Maybe ()) where
    -- This is injective because the type () has only one value.
    inject _ = Nothing
```

However, we also require instances to be _canonical_.
This instance isn't canonical because it arbitrarily restricts @from ~ ()@.
Actually, there is already the definition of a canonical injection into `Maybe`;
we may as well write

```.hs
instance Injection a (Maybe a) where
    inject = pure
```

One consequence of the injectivity law is that the output type must be at least as large as the input type.
We can inject a `Maybe a` into a `[a]` because the latter type is strictly larger:

```.hs
instance Injection (Maybe a) [a] where
    inject = maybeToList
```

Where `Maybe a` contains either zero or one values of `a`, `[a]` contains zero or more values of `a`.

Some common conversions are notably _not_ injective.
For example, `Data.Map.fromList` returns the same `Map` for different lists:

```.hs
Data.Map.fromList [('a', 'A'), ('b', 'B')] == Data.Map.fromList [('b', 'B'), ('a', 'A')]
```

Therefore, we cannot define an `instance [(k, v)] (Map k v)`.

When there is an equivalence between two types, that equivalence is usually an injection.
For example, the class `Integral` defines

```.hs
toInteger :: Integra a => a -> Integer
```

Where this conversion is a total equivalence, it forms a canonical injection into `Integer`:

```.hs
instance Injection Natural Integer where
    inject = toInteger
```

Likewise, the class `Num` defines an equivalence

```.hs
fromInteger :: Num a => Integer -> a
```

For types that have total implementations of `fromInteger`, this is usually an injection:

```.hs
instance HasResolution a => Injection Integer (Fixed a) where
    inject = fromInteger
```

```.hs
-- BAD: No reasonable person would accept this!
instance Injection String Text where
    inject = Text.pack . reverse
```

## Retraction

Because `Injection` is a lossless conversion, we can define a `Retraction` which undoes it.
The method

```.hs
retract :: into -> Maybe from
```

is the (left) inverse of `inject`:

```.hs
retract (inject x) = Just x
```

`retract` is partial (returns `Maybe`) because the type `into` may be larger than the type `from`;
that is, there may be values in `into` which are not `inject`-ed from `from`,
and in that case `retract` may return `Nothing`.

### Examples

```.hs
instance Typeable a => Retraction a Dynamic where
    retract = fromDyn
```

```.hs
instance Retraction a (Maybe a) where
    retract = id
```

```.hs
instance Retraction (Maybe a) [a] where
    retract [] = Just Nothing
    retract [x] = Just (Just x)
    retract _ = Nothing
```

```.hs
instance Retraction Natural Integer where
    retract x
        | x < 0 = Nothing
        | otherwise = Just (fromInteger x)
```
