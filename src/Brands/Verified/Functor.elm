module Brands.Verified.Functor exposing (..)

import Brands exposing (..)
import Brands.Leibniz exposing (..)

type alias VerifiedFunctor f a b =
  { map : (a -> b) -> App f a -> App f b
  , laws :
    { identity : Eq (((a -> b) -> App f a -> App f b) -> (a -> a) -> App f a) ((a -> a) -> App f a)
    }
  }

map : VerifiedFunctor f a b -> (a -> b) -> App f a -> App f b
map  =
  .map

set : VerifiedFunctor f a b -> b -> App f a -> App f b
set { map } x f =
  map (always x) f

void : VerifiedFunctor f a () -> App f a -> App f ()
void functor =
  set functor ()
