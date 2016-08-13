module Brands.Semigroup exposing (..)

type alias Semigroup m =
  { concat : m -> m -> m
  }

concat : Semigroup m -> m -> m -> m
concat =
  .concat
