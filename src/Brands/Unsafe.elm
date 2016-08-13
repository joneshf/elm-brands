module Brands.Unsafe exposing (..)

import Brands exposing (..)
import Native.Unsafe

inj : a -> App f b
inj =
  Native.Unsafe.coerce

prj : App f b -> a
prj =
  Native.Unsafe.coerce
