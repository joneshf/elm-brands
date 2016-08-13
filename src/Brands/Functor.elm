module Brands.Functor exposing (..)

import Brands exposing (..)

type alias Functor f a b =
  { map : (a -> b) -> App f a -> App f b
  }

map : Functor f a b -> (a -> b) -> App f a -> App f b
map  =
  .map

set : Functor f a b -> b -> App f a -> App f b
set { map } x f =
  map (always x) f

void : Functor f a () -> App f a -> App f ()
void functor =
  set functor ()
