module Brands.Embed exposing (..)

import Brands exposing (..)
import Brands.Unsafe exposing (..)

type Embed f a =
  Embed (App f a)

embed : App f a -> Embed f a
embed =
  Embed

run : Embed f a -> App f a
run (Embed x) =
  x

inj : a -> Embed f a
inj =
  embed << Brands.Unsafe.inj

prj : Embed f a -> a
prj =
  Brands.Unsafe.prj << run
