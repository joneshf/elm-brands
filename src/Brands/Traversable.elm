module Brands.Traversable exposing (..)

import Brands exposing (..)
import Brands.Applicative exposing (..)
import Brands.Monoid exposing (..)

type alias Traversable t f m a b =
  { map : (a -> b) -> App f a -> App f b
  , foldMap : Monoid m -> (a -> m) -> App f a -> m
  , traverse : Applicative f a b -> (a -> App f b) -> App t a -> App f (App t b)
  }

traverse : Traversable t f m a b -> Applicative f a b -> (a -> App f b) -> App t a -> App f (App t b)
traverse =
  .traverse

sequence : Traversable t f m (App f a) a -> Applicative f (App f a) a -> App t (App f a) -> App f (App t a)
sequence t f =
  traverse t f identity

for : Traversable t f m a b -> Applicative f a b -> App t a -> (a -> App f b) -> App f (App t b)
for t f =
  flip (traverse t f)
