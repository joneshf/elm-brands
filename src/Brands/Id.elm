module Brands.Id exposing (..)

import Brands exposing (..)
import Brands.Functor exposing (..)
import Brands.Foldable exposing (..)

functor : Functor Id a b
functor =
  { map = Brands.map
  }

foldable : Foldable Id a m
foldable =
  { foldMap = \_ f (App _ x) ->
    f x
  }

type Id
  = Id

inj : a -> App Id a
inj x =
  App Id x

prj : App Id a -> a
prj (App _ x) =
  x
