module Brands.Applicative exposing (..)

import Brands exposing (..)

type alias Applicative f a b =
  { map : (a -> b) -> App f a -> App f b
  , ap : App f (a -> b) -> App f a -> App f b
  , pure : a -> App f a
  }

pure : Applicative f a b -> a -> App f a
pure  =
  .pure

when : Applicative f () b -> Bool -> App f () -> App f ()
when { pure } b f =
  if b then
    f
  else
    pure ()
