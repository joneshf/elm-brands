module Brands.Apply exposing (..)

import Brands exposing (..)

type alias Apply f a b =
  { map : (a -> b) -> App f a -> App f b
  , ap : App f (a -> b) -> App f a -> App f b
  }

ap : Apply f a b -> App f (a -> b) -> App f a -> App f b
ap  =
  .ap

lift2 :
  { r
  | ap : App f (b -> c) -> App f b -> App f c
  , map : (a -> b -> c) -> App f a -> App f (b -> c)
  }
  -> (a -> b -> c)
  -> App f a
  -> App f b
  -> App f c
lift2 {ap, map} f fa fb =
  ap (map f fa) fb
