module Brands.Foldable exposing (..)

import Brands exposing (..)
import Brands.Monoid exposing (..)

type alias Foldable f a m =
  { foldMap : Monoid m -> (a -> m) -> App f a -> m
  }

foldMap : Foldable f a m -> Monoid m -> (a -> m) -> App f a -> m
foldMap =
  .foldMap

length : Foldable f a (Sum Int) -> App f a -> Int
length { foldMap } =
  runSum << foldMap monoidSum (\_ -> Sum 1)

toList : Foldable f a (List a) -> App f a -> List a
toList { foldMap } =
  foldMap monoidList (\x -> [x])

isEmpty : Foldable f a All -> App f a -> Bool
isEmpty { foldMap } =
  runAll << foldMap monoidAll (\_ -> All False)

member : Foldable f a Any -> a -> App f a -> Bool
member { foldMap } x =
  runAny << foldMap monoidAny (\y -> if x == y then Any True else Any False)

maximum : Foldable f comparable (Max comparable) -> App f comparable -> Maybe comparable
maximum { foldMap } =
  runMax << foldMap monoidMax (Max << Just)

minimum : Foldable f comparable (Min comparable) -> App f comparable -> Maybe comparable
minimum { foldMap } =
  runMin << foldMap monoidMin (Min << Just)

type Sum number
  = Sum number

runSum : Sum number -> number
runSum (Sum x) =
  x

monoidSum : Monoid (Sum number)
monoidSum =
  { empty = Sum 0
  , concat = \(Sum x) (Sum y) ->
    Sum (x + y)
  }

monoidList : Monoid (List a)
monoidList =
  { empty = []
  , concat = (++)
  }

type All
  = All Bool

runAll : All -> Bool
runAll (All p) =
  p

monoidAll : Monoid All
monoidAll =
  { empty = All True
  , concat = \(All p) (All q) ->
    All (p && q)
  }

type Any
  = Any Bool

runAny : Any -> Bool
runAny (Any p) =
  p

monoidAny : Monoid Any
monoidAny =
  { empty = Any False
  , concat = \(Any p) (Any q) ->
    Any (p || q)
  }

type Max comparable
  = Max (Maybe comparable)

runMax : Max comparable -> Maybe comparable
runMax (Max x) =
  x

monoidMax : Monoid (Max comparable)
monoidMax =
  { empty = Max Nothing
  , concat = \(Max x) (Max y) ->
    case (x, y) of
      (Just m, Just n) ->
        Max (Just (max m n))
      (Nothing, _) ->
        Max y
      (_, Nothing) ->
        Max x
  }

type Min comparable
  = Min (Maybe comparable)

runMin : Min comparable -> Maybe comparable
runMin (Min x) =
  x

monoidMin : Monoid (Min comparable)
monoidMin =
  { empty = Min Nothing
  , concat = \(Min x) (Min y) ->
    case (x, y) of
      (Just m, Just n) ->
        Min (Just (min m n))
      (Nothing, _) ->
        Min y
      (_, Nothing) ->
        Min x
  }
