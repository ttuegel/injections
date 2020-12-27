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

Actually, constructors with a single argument are always injective functions.
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
We feel that this instance is not canonical because it is not written to accept the widest possible range of types.

One consequence of the injectivity law is that the output type must be at least as large as the input type.
We can inject a `Maybe a` into a `[a]` because the latter type is strictly larger:

```.hs
instance Injection (Maybe a) [a] where
    inject = maybeToList
```

Where `Maybe a` contains either zero or one values of `a`, `[a]` contains zero or more values of `a`.

Likewise, there is a canonical injection from the natural numbers (non-negative integers) to the integers:

```.hs
instance Injection Natural Integer where
    inject = toInteger
```
