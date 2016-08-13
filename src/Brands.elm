module Brands exposing (..)

type App f a
  = App f a

map : (a -> b) -> App f a -> App f b
map g (App f x) =
  App f (g x)

extract : App f a -> a
extract (App _ x) =
  x
