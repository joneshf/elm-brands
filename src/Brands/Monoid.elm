module Brands.Monoid exposing (..)

type alias Monoid m =
  { empty : m
  , concat : m -> m -> m
  }

empty : Monoid m -> m
empty =
  .empty
