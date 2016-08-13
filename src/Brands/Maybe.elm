module Brands.Maybe exposing (..)

import Brands exposing (..)
import Brands.Apply exposing (..)
import Brands.Applicative exposing (..)
import Brands.Foldable exposing (..)
import Brands.Functor exposing (..)
import Brands.Unsafe exposing (..)

type MaybeB
  = MaybeB

functor : Functor MaybeB a b
functor =
  { map = \f ->
    inj << Maybe.map f << prj
  }

foldable : Foldable MaybeB a m
foldable =
  { foldMap = \{ empty } f m ->
    case prj m of
      Just x ->
        f x
      Nothing ->
        empty
  }

apply : Apply MaybeB a b
apply =
  { map = functor.map
  , ap = \mf mx ->
    case (prj mf, prj mx) of
      (Just f, Just x) ->
        inj (Just (f x))
      _ ->
        inj Nothing
  }

applicative : Applicative MaybeB a b
applicative =
  { map = functor.map
  , ap = apply.ap
  , pure = inj << Just
  }

inj : Maybe a -> App MaybeB a
inj =
  Brands.Unsafe.inj

prj : App MaybeB a -> Maybe a
prj =
  Brands.Unsafe.prj
