module Brands.Flip exposing (..)

import Brands exposing (..)
import Brands.Unsafe exposing (..)

type Flip f a b =
  Flip (App (App f b) a)

runFlip : Flip f a b -> App (App f b) a
runFlip (Flip f) =
  f

inj : Flip f a b -> App (App f a) b
inj =
  Brands.Unsafe.inj

prj : App (App f a) b -> Flip f a b
prj =
  Brands.Unsafe.prj
