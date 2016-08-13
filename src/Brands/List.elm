module Brands.List exposing (ListB, foldable, functor, inj, prj, traversable)

import Brands exposing (..)
import Brands.Apply exposing (..)
import Brands.Foldable exposing (..)
import Brands.Functor exposing (..)
import Brands.Monoid exposing (..)
import Brands.Unsafe exposing (..)

type ListB
  = ListB

functor : Functor ListB a b
functor =
  { map = \f ->
    inj << List.map f << prj
  }

foldable : Foldable ListB a m
foldable =
  { foldMap = foldMap
  }

foldMap : Monoid m -> (a -> m) -> App ListB a -> m
foldMap ({concat, empty} as monoid) f xs =
  case prj xs of
    [] ->
      empty
    x :: xss ->
      concat (f x) (foldMap monoid f (inj xss))

--traversable : Traversable ListB f m a b
traversable :
  { foldMap : Monoid m -> (a -> m) -> App ListB a -> m
  , map : (a -> b) -> App ListB a -> App ListB b
  , traverse :
    { ap : App f (App ListB b -> App ListB b) -> App f (App ListB b) -> App f (App ListB b)
    , map : (b -> App ListB b -> App ListB b) -> App f b -> App f (App ListB b -> App ListB b)
    , pure : App ListB b -> App f (App ListB b)
    }
    -> (a -> App f b)
    -> App ListB a
    -> App f (App ListB b)
  }
traversable =
  { map = functor.map
  , foldMap = foldable.foldMap
  , traverse = traverse
  }

traverse :
  { ap : App f (App ListB b -> App ListB b) -> App f (App ListB b) -> App f (App ListB b)
  , map : (b -> App ListB b -> App ListB b) -> App f b -> App f (App ListB b -> App ListB b)
  , pure : App ListB b -> App f (App ListB b)
  }
  -> (a -> App f b)
  -> App ListB a
  -> App f (App ListB b)
traverse ({ pure } as applicative) f xs =
  case prj xs of
    [] ->
      pure (inj [])
    x :: xss ->
      lift2 applicative cons (f x) (traverse applicative f (inj xss))

cons : b -> App ListB b -> App ListB b
cons y ys =
  inj (y :: prj ys)

inj : List a -> App ListB a
inj =
  Brands.Unsafe.inj

prj : App ListB a -> List a
prj =
  Brands.Unsafe.prj
