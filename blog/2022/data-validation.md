---
title: "Functional data validation: to monad or not to monad?"
author: "Matthias Valvekens"
tags: [functional-programming, haskell, tech]
published: 2022-12-17
license: "CC BY-SA 4.0 hybrid"
---


# Problem statement


"Make invalid states unrepresentable" is an oft-repeated adage in software design, particularly popular in languages with expressive type systems. It's good advice, and it generally leads to cleaner code bases with fewer opportunities for bugs to creep in.

However, the programs we write are not idealised spherical cows in a vacuum: they actually have to _communicate_ with their environment to do anything useful, e.g. through a REST API, a streaming service or some other I/O layer. Maybe you're sending data over the wire using a protocol with strict backwards compatibility requirements, or your database schema has changed over the years, and you have to find ways to cope with potential defects in historical data. We can't let these earthly concerns get in the way of modelling our computations properly now, can we?

Briefly put, in order for the internal domain model to be "nice", one typically has to do a lot of data conversion and validation at the edge of the system.
Last week, I found myself in such a situation, which led to a discussion on the merits and demerits of various FP-based error handling techniques. That discussion is what prompted me to write this post.


This validation/conversion problem is not a new or unsolved problem in FP, but some design choices need to be made:

 - Do we want the data ingestion process to fail fast when encountering errors, or do we rather want to try to process as much data as possible and report the resulting errors?
 - Do we want to distinguish between errors and warnings?
 - Is the validation logic required to compose cleanly with the way we handle errors?

In this post, we'll take a look at a few patterns to tackle these questions in a functional programming setting, with plenty of example code. I'll be using Haskell for the examples, but they should work just as well in other languages with a decent type system like Scala or OCaml.


# A toy example

Let's imagine a situation where we're processing information about human users of some application, with a domain model looking like this:

```haskell
import Data.Time.Calendar

data User = User
  { name :: String,
    dateJoined :: Day,
    dateOfBirth :: Maybe Day
  }
  deriving (Show)
```

In other words, for every user we record a name and the date they joined, and possibly their date of birth, if applicable. Next, let's imagine that we have to extract this user data from a `Map String String`. Obviously, this is a contrived example, but it'll do for now.

What about errors? To keep things simple, let's say that we are interested in two error condition: missing required fields, and parse errors in fields. Those can be represented in the following error type:


```haskell
type FieldName = String  -- convenient alias for readability

data ConversionErr
  = MissingField FieldName
  | FieldParsingError FieldName String
  deriving (Show)
```


# Warm-up: error handling with `Either`

The most basic error handler in functional programming is arguably `Either`.

Applied to our toy example, this is what the API looks like:

```haskell

type FieldData = Map String String

toUser :: FieldData -> Either ConversionErr User
```

In plain terms, the `toUser` function takes some field data as input, and spits out either a `ConversionErr`, or a `User` value.

Here's an example module implementing this `toUser` API.


```haskell
module ExampleAbstract
  ( User (..),
    ConversionErr (..),
    FieldName,
    FieldParser,
    FieldData,
    ConvertWithErrors (..),
    requiredField,
    optionalField,
    parseDate,
    toUser,
  )
where


import Data.Map
import Data.Time.Calendar (Day)
import Data.Time.Format.ISO8601 (iso8601ParseM)
import Prelude hiding (fail)

data User = User
  { name :: String,
    dateJoined :: Day,
    dateOfBirth :: Maybe Day
  }
  deriving (Show)

type FieldName = String

data ConversionErr
  = MissingField FieldName
  | FieldParsingError FieldName String
  deriving (Show)

type FieldData = Map String String

type FieldParser a = String -> Either String a


-- | Parse an ISO date string into a 'Day' value.
parseDate :: FieldParser Day
parseDate str = case iso8601ParseM str of
  Just day -> Right day
  Nothing -> Left $ str ++ " is not a valid date string"


-- | Look up a required field and parse it using the provided 'FieldParser'.
requiredField ::
  FieldName ->
  FieldParser a ->
  FieldData ->
  Either ConversionErr a
requiredField fieldName parse input = case lookup fieldName input of
  Just value -> first (FieldParsingError fieldName) (parse value)
  Nothing -> Left (MissingField fieldName)


-- | Look up an optional field and parse it using the provided 'FieldParser'
-- if a value is found. If thoe field is not present, return 'Nothing'.
-- If the field can't be parsed, return an error.
optionalField ::
  FieldName ->
  FieldParser a ->
  FieldData ->
  Either ConversionErr (Maybe a)
optionalField fieldName parse input = case lookup fieldName input of
  Just value -> Just <$> first (FieldParsingError fieldName) (parse value)
  Nothing -> Right Nothing


-- | Construct a 'User' from 'FieldData'.
toUser :: FieldData -> Either ConversionErr User
toUser input = User <$> getName <*> getDateJoined <*> getDateOfBirth
  where
    getName = requiredField "name" pure input
    getDateJoined = requiredField "dateJoined" parseDate input
    getDateOfBirth = optionalField "dateOfBirth" parseDate input
```


Notice how the `toUser` function actually only uses `Applicative`-style combinators. This abstraction will be useful later.
But first, let's try this out on some inputs.

```
ghci> toUser $ fromList [("name", "John Doe"), ("dateJoined", "2022-12-14")]
Right (User {name = "John Doe", dateJoined = 2022-12-14, dateOfBirth = Nothing})
ghci> toUser $ fromList [("name", "John Doe"), ("dateJoined", "2022-12-14"), ("dateOfBirth", "1960-01-01")]
Right (User {name = "John Doe", dateJoined = 2022-12-14, dateOfBirth = Just 1960-01-01})
ghci> toUser $ fromList [("name", "John Doe"), ("dateJoined", "2022-12-32")]
Left (FieldParsingError "dateJoined" "2022-12-32 is not a valid date string")
ghci> toUser $ fromList [("name", "John Doe"), ("dateOfBirth", "1960-01-01")]
Left (MissingField "dateJoined")
ghci> toUser $ fromList [("name", "John Doe"), ("dateOfBirth", "1960-01-32")]
Left (MissingField "dateJoined")
```

Some observations:

 - When the input is valid, we get back a `Right (User {...})` value.
 - When the input is not valid, we get back a `Left (...)` with a `ConversionErr` value that tells us what went wrong.
 - `Either` fails fast: it will stop processing input on the first error it encounters. As a consequence of that, you'll always see at most one error in the final `Either` value, even if the input has multiple issues!


While `Either` is useful to get syntactically cheap error messages, its "fail-fast" behaviour is not always desirable. For example, if you're processing data submitted by a human user, having them fix the input one error at a time doesn't exactly make for a great user experience. Can we do better?


# Abstract setup

Before we proceed to experiment with alternative approaches to error management, let's rewrite that code a little more abstractly, so we can easily swap between error handling strategies.


```haskell
module ExampleAbstract where

import qualified Data.Map as M
import Data.Time.Calendar (Day)
import Data.Time.Format.ISO8601 (iso8601ParseM)
import Prelude hiding (fail, lookup)

data User = User
  { name :: String,
    dateJoined :: Day,
    dateOfBirth :: Maybe Day
  }
  deriving (Show)

type FieldName = String

data ConversionErr
  = MissingField FieldName
  | FieldParsingError FieldName String
  deriving (Show)

type FieldData = M.Map String String

class Applicative f => ConvertWithErrors f where
  fail :: ConversionErr -> f a

parseField :: ConvertWithErrors f => FieldName -> Either String a -> f a
parseField _ (Right result) = pure result
parseField fieldName (Left err) = fail (FieldParsingError fieldName err)

type FieldParser a = String -> Either String a

-- | Parse an ISO date string into a 'Day' value.
parseDate :: FieldParser Day
parseDate str = case iso8601ParseM str of
  Just day -> Right day
  Nothing -> Left $ str ++ " is not a valid date string"

-- | Look up a required field and parse it using the provided 'FieldParser'.
requiredField ::
  ConvertWithErrors f =>
  FieldName ->
  FieldParser a ->
  FieldData ->
  f a
requiredField fieldName parse input = case M.lookup fieldName input of
  Just value -> parseField fieldName (parse value)
  Nothing -> fail (MissingField fieldName)

-- | Look up an optional field and parse it using the provided 'FieldParser'
-- if a value is found. If the field is not present, return 'Nothing'.
-- If the field can't be parsed, return an error.
optionalField ::
  ConvertWithErrors f =>
  FieldName ->
  FieldParser a ->
  FieldData ->
  f (Maybe a)
optionalField fieldName parse input = case M.lookup fieldName input of
  Just value -> Just <$> parseField fieldName (parse value)
  Nothing -> pure Nothing

-- | Construct a 'User' from 'FieldData'.
toUser :: ConvertWithErrors f => FieldData -> f User
toUser input = User <$> getName <*> getDateJoined <*> getDateOfBirth
  where
    getName = requiredField "name" pure input
    getDateJoined = requiredField "dateJoined" parseDate input
    getDateOfBirth = optionalField "dateOfBirth" parseDate input

```

A few remarks:

 - Notice how `ConvertWithErrors` is defined to extend `Applicative` instead of `Monad`. This is actually key to our approach.

 - For simplicity's sake, we kept the `Either String a` type for parsing individual fields. If you're parsing nested structures, you might want to track errors across multiple "levels". This requires more sophisticated typing.


To recover our original `toUser`, it now suffices to write

```haskell
instance ConvertWithErrors (Either ConversionErr) where
  fail = Left
```

# Gathering multiple errors with `Validated`

Recall how `Either` did not allow us to return _all_ the errors in a computation at once. This is a natural consequence of `Either`'s fail-fast approach.

The `Validated` functor (found in [Scala's `cats` suite](https://typelevel.org/cats/datatypes/validated.html), among others) doesn't have that limitation. Typically, it's defined as follows:


```haskell
data Validated e a = Invalid e | Valid a deriving (Show)

valid :: Validated e a -> Maybe a
valid (Valid x) = Just x
valid _ = Nothing

invalid :: Validated e a -> Maybe e
invalid (Invalid err) = Just err
invalid _ = Nothing


instance Functor (Validated e) where
    fmap f (Valid x) = Valid (f x)
    fmap _ (Invalid e) = Invalid e

instance Semigroup e => Applicative (Validated e) where
    pure = Valid

    Valid f <*> Valid x = Valid (f x)
    Invalid err <*> Valid _ = Invalid err
    Invalid err <*> Invalid err' = Invalid (err <> err')
    Valid _ <*> Invalid err = Invalid err
```


The definition of `Validated` as a type looks very similar to `Either`. In fact, the two are isomorphic as types (and as (bi)functors).

The fundamental difference is in the `Applicative` instance:

 - Unlike `Either e`, `Validated e` has a `Semigroup` constraint on the error type `e`.This is necessary to combine the errors!
 - The implementation of `<*>` is different: with `Either`, `Left err <*> Left err'` would always yield `Left err`, while `Validated` allows the two errors to be combined using the `Semigroup` instance for `e`.
 - Perhaps less obvious is the fact that the `Applicative` instance for `Validated e` is actually **not monadic**! In other words, there is no way to (polymorphically in `e`[^trivial-semigroup]) define an instance of `Monad (Validated e)` such that the `Applicative` structure induced by `(<*>) = ap` reduces to the one defined here!


[^trivial-semigroup]: Unless the `Semigroup` instance on `e` is sufficiently degenerate, that is. For example, in the case where `e = ()`, we of course have `Either () ≅ Validated ()` as applicatives.


Let's try to apply this to our `toUser` example.


```haskell
import qualified Data.Map as M
import qualified Data.List.NonEmpty as L
import ExampleAbstract

instance ConvertWithErrors (Validated (L.NonEmpty ConversionErr)) where
  fail err = Invalid (L.singleton err)


testUserValidated :: [(String, String)] -> IO ()
testUserValidated fields = case toUser (M.fromList fields) of
  Valid usr -> print usr
  Invalid (errs :: L.NonEmpty ConversionErr) -> sequence_ (print <$> errs)

```

:::info
We use `NonEmpty ConversionErr` instead of the perhaps more straightforward `[ConversionErr]` because `Invalid []` is a nonsensical result state. We don't want those in our data model.

This is also why the `Semigroup`'s used for error handling are typically not `Monoid`s.
:::


Running this on some invalid input indeed shows that we now get back all the errors, as expected:

```
ghci> testUserValidated [("name", "John Doe"), ("dateJoined", "2020-12-31")]
User {name = "John Doe", dateJoined = 2020-12-31, dateOfBirth = Nothing}
ghci> testUserValidated [("dateJoined", "2020-12-32"), ("dateOfBirth", "2000-13-01")]
MissingField "name"
FieldParsingError "dateJoined" "2020-12-32 is not a valid date string"
FieldParsingError "dateOfBirth" "2000-13-01 is not a valid date string"
```


Here's what we have so far:

 - With a simple change in types, we got more aggressive validation logic without having to change any of the "business logic".
 
 - The price we paid for that is that our error-gathering type is no longer a monad, but "merely" an applicative. Fortunately, in practice, many interesting computations where one would want to use this type of error handling can be restructured to not really require monads in the first place.


# Recoverable errors with `Ior`



What if you want to be able to distinguish between critical errors and recoverable ones? In our `toUser` example, the `dateOfBirth` field is a `Maybe Day`, so we could elect to just set it to `Nothing` if the input is invalid, while still logging a warning.

Let's start by extending our example a little to spell out what we want.

```haskell
le ExampleAbstractWarnings
  ( module ExampleAbstract,
    ConvertWithWarnings (..),
    toUserTolerant,
  )
where

import qualified Data.Map as M
import ExampleAbstract

class ConvertWithErrors f => ConvertWithWarnings f where
  -- | Record a warning and return a default value.
  warn :: ConversionErr -> a -> f a

parseFieldOrDefault ::
  ConvertWithWarnings f =>
  FieldName ->
  a ->
  Either String a ->
  f a
parseFieldOrDefault _ _ (Right x) = pure x
parseFieldOrDefault fieldName defResult (Left err) =
  warn (FieldParsingError fieldName err) defResult

tolerantOptionalField ::
  ConvertWithWarnings f =>
  FieldName ->
  FieldParser a ->
  FieldData ->
  f (Maybe a)
tolerantOptionalField fieldName parse input = case M.lookup fieldName input of
  Just value -> parseFieldOrDefault fieldName Nothing (Just <$> parse value)
  Nothing -> pure Nothing

-- | Construct a 'User' from 'FieldData'.
toUserTolerant :: ConvertWithWarnings f => FieldData -> f User
toUserTolerant input = User <$> getName <*> getDateJoined <*> getDateOfBirth
  where
    getName = requiredField "name" pure input
    getDateJoined = requiredField "dateJoined" parseDate input
    getDateOfBirth = tolerantOptionalField "dateOfBirth" parseDate input
```


One way to implement this new `ConvertWithWarnings` typeclass goes through the `Ior` ("inclusive or") type, [which is also part of Scala's `cats` toolkit](https://typelevel.org/cats/datatypes/ior.html). In a nutshell, `Ior` is a variant of `Either` with an added `Both` constructor.

```haskell
data Ior a b = JustLeft a | JustRight b | Both a b deriving (Show)

instance Functor (Ior a) where
  fmap _ (JustLeft x) = JustLeft x
  fmap f (JustRight y) = JustRight (f y)
  fmap f (Both x y) = Both x (f y)
```

In order to define a suitable `Applicative` instance, we again need the error type to be a `Semigroup`, like with `Validated`.

```haskell

-- | Utility function to "attach" existing errors to a continued computation.
combineErrors :: Semigroup e => e -> Ior e a -> Ior e a
combineErrors err (JustRight x) = Both err x
combineErrors err (JustLeft err') = JustLeft (err <> err')
combineErrors err (Both err' y) = Both (err <> err') y


instance Semigroup e => Applicative (Ior e) where
  pure = JustRight

  JustLeft err <*> _ = JustLeft err
  JustRight f <*> xF = fmap f xF
  Both err f <*> xF = combineErrors err (fmap f xF)
```

Actually, we didn't even have to bother spelling out the `(<*>)` implementation, because `Ior e` is actually a monad[^iort] under this constraint! We could just as well have written


```haskell
instance Semigroup e => Applicative (Ior e) where
    pure = JustRight
    (<*>) = ap   -- from Control.Monad

instance Semigroup e => Monad (Ior e) where
  JustLeft err >>= _ = JustLeft err
  JustRight x >>= f = f x
  Both err x >>= f = combineErrors err (f x)
```

[^iort]: Even better, it's not hard to write down a monad transformer instance for `data IorT e m a = IorT (m (Ior e a))` such that `Ior e ≅ IorT e Identity`.


We now pretty much have everything we need.


```haskell
import qualified Data.List.NonEmpty as L
import qualified Data.Map as M
import ExampleAbstractWarnings

instance ConvertWithErrors (Ior (L.NonEmpty ConversionErr)) where
  fail err = JustLeft (L.singleton err)

instance ConvertWithWarnings (Ior (L.NonEmpty ConversionErr)) where
  warn err defResult = Both (L.singleton err) defResult


testUserIor :: [(String, String)] -> IO ()
testUserIor fields = case toUserTolerant (M.fromList fields) of
    JustRight usr -> print usr
    JustLeft errs -> printErrs errs
    Both warnings usr -> print usr >> putStrLn "Warnings:" >> printErrs warnings
  where printErrs :: L.NonEmpty ConversionErr -> IO ()
        printErrs errs = sequence_ (print <$> errs)
```

Let's try that on some inputs as well:

```
ghci> testUserIor [("name", "John Doe"), ("dateJoined", "2020-12-31"), ("dateOfBirth", "2000-13-01")]
User {name = "John Doe", dateJoined = 2020-12-31, dateOfBirth = Nothing}
Warnings:
FieldParsingError "dateOfBirth" "2000-13-01 is not a valid date string"
ghci> testUserIor [("name", "John Doe"), ("dateJoined", "2020-12-31")]
User {name = "John Doe", dateJoined = 2020-12-31, dateOfBirth = Nothing}
ghci> testUserIor [("dateJoined", "2020-12-32"), ("dateOfBirth", "2000-13-01")]
MissingField "name"
```


So, we've now gotten our monad property back, and we're able to distinguish between errors and warnings. However, the third invocation shows that, like `Either`, `Ior` also fails fast on critical errors! What if that's not what you want?



# Aggregating both critical and recoverable errors

Let's try to figure out how to define an applicative that is to `Ior` as `Validated` is to `Either`.
Unlike the previous examples, I haven't actually encountered this one in any libraries yet, but maybe I didn't look hard enough.

Given what we've done so far, it seems reasonable to start with something isomorphic to `Ior`.

```haskell
data Pedantic e a = Accept a | Reject e | Nitpick e a deriving (Show)

accepted :: Pedantic e a -> Maybe a
accepted (Accept x) = Just x
accepted (Nitpick _ x) = Just x
accepted _ = Nothing

errors :: Pedantic e a -> Maybe e
errors (Reject err) = Just err
errors (Nitpick err _) = Just err
errors _ = Nothing

instance Functor (Pedantic e) where
  fmap f (Accept x) = Accept (f x)
  fmap _ (Reject err) = Reject err
  fmap f (Nitpick err x) = Nitpick err (f x)
```


In order to get inspiration for a sensible `Applicative` instance for `Pedantic e`, it is instructive to note that the `<*>` implementations for `Validated e` and `Ior e` can be rewritten in a more point-free style as follows.

```haskell
instance Semigroup e => Applicative (Validated e) where
  pure = Valid

  -- This relation is forced by the applicative laws.
  (<*>) (Valid f) = fmap f

  -- What this says informally is that applying (Invalid err <*>), we know we 
  -- always end up with an Invalid state in the end, but we grab 
  -- remaining errors (if any!) first before we return it.

  (<*>) (Invalid err) = Invalid . maybe err (err <>) . invalid


instance Semigroup e => Applicative (Ior e) where
  pure = JustRight

  -- Again: applicative laws.
  (<*>) (JustRight f) = fmap f
  -- Fail immediately without bothering to evaluate the second argument. 
  (<*>) (JustLeft err) = const (JustLeft err)
  -- Apply the function to what comes next, and tack on the errors we already 
  -- accumulated after evaluating the result.
  (<*>) (Both err f) = combineErrors err . fmap f
```


Combining these, the `Applicative (Pedantic e)` instance we were looking for is now pretty obvious.

```haskell
addPedantry :: Semigroup e => e -> Pedantic e a -> Pedantic e a
addPedantry err (Accept x) = Nitpick err x
addPedantry err (Reject err') = Reject (err <> err')
addPedantry err (Nitpick err' x) = Nitpick (err <> err') x

instance Semigroup e => Applicative (Pedantic e) where
  pure = Accept

  (<*>) (Accept f) = fmap f
  (<*>) (Reject err) = Reject . maybe err (err <>) . errors
  (<*>) (Nitpick err f) = addPedantry err . fmap f
```

::: info
At this point, you could also decide to unify `Accept` and `Nitpick` by having `Nitpick` take a `Maybe e` instead of an `e` as its first argument. That's ultimately a matter of taste.
:::


The identity, homomorphism and interchange laws are easy to verify. Checking the composition law requires some effort, but the proof is an ultimately straightforward exercise in symbol gymnastics.
However, as was the case with `Validated`, the `Applicative` instance for `Pedantic` is not monadic.


::: info

If you want to sit down and prove the composition law for `Pedantic`, here's a hint: verify the following identities first.

```haskell
f <$> (v <*> w) = ((f.) <$> v) <*> w
errors (v <*> w) = errors v <> errors w   -- (Maybe e) is a Semigroup too!
(addPedantry err v <*> w) = addPedantry err (v <*> w)
```

It goes without saying that the associativity of `(<>)` is essential to the argument.
:::

To test out our fancy new `Applicative`, let's implement our test example with it.

```haskell
import qualified Data.List.NonEmpty as L
import qualified Data.Map as M
import ExampleAbstractWarnings


instance ConvertWithErrors (Pedantic (L.NonEmpty ConversionErr)) where
  fail err = Reject (L.singleton err)

instance ConvertWithWarnings (Pedantic (L.NonEmpty ConversionErr)) where
  warn err defResult = Nitpick (L.singleton err) defResult

testUserPedantic :: [(String, String)] -> IO ()
testUserPedantic fields = case toUserTolerant (M.fromList fields) of
  Accept usr -> print usr
  Reject errs -> printErrs errs
  Nitpick warnings usr ->
    print usr >> putStrLn "Warnings:" >> printErrs warnings
  where
    printErrs :: L.NonEmpty ConversionErr -> IO ()
    printErrs errs = sequence_ (print <$> errs)
```


Running this test function against the usual set of inputs then produces the following:

```
ghci> testUserPedantic [("name", "John Doe"), ("dateJoined", "2020-12-31")]
User {name = "John Doe", dateJoined = 2020-12-31, dateOfBirth = Nothing}
ghci> testUserPedantic [("name", "John Doe"), ("dateJoined", "2020-12-31"), ("dateOfBirth", "2000-13-01")]
User {name = "John Doe", dateJoined = 2020-12-31, dateOfBirth = Nothing}
Warnings:
FieldParsingError "dateOfBirth" "2000-13-01 is not a valid date string"
ghci> testUserPedantic [("dateJoined", "2020-12-32"), ("dateOfBirth", "2000-13-01")]
MissingField "name"
FieldParsingError "dateJoined" "2020-12-32 is not a valid date string"
FieldParsingError "dateOfBirth" "2000-13-01 is not a valid date string"
```

That's it! We have gotten rid of the fail-fast behaviour while also keeping the distinction between critical and recoverable errors!

::: info
Taking all of this one step further, one could even rework the typing a little so that errors and warnings are tracked completely separately, e.g. to report them separately to a human user.

Here's what that type definition might look like:

```haskell
data Pedantic' e w a =  Nitpick' (Maybe w) a | Reject' (Maybe w) e
```

Writing down the appropriate `Applicative (Pedantic' e w)` instance is left as an exercise to the reader.
:::


# Wrapping up


We've covered several strategies for handling error conditions here, each with their own advantages and drawbacks.

 - `Either` and `Ior` are monadic and fail fast on unrecoverable errors. They're also monad transformers, which makes them easy to use in code that already makes heavy use of monad transformer stacks.
 - `Validated` and `Pedantic` never fail fast, and will attempt to gather as much error information as the structure of the computation allows. They can't be made into monads, though.
 - `Ior` and `Pedantic` have a recover-with-warning mechanism built in, but can also be used to model unrecoverable error behaviour.
 - `Ior`, `Validated` and `Pedantic` require the error type to supply a `Semigroup` instance. `Either` doesn't care.


