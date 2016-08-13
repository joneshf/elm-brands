module Brands.Html exposing (..)

import Html exposing (Html, div, text)
import Html.App

import Brands exposing (..)
import Brands.Functor exposing (..)
import Brands.Monoid exposing (..)
import Brands.Semigroup exposing (..)
import Brands.Unsafe exposing (..)

type HtmlB
  = HtmlB

functor : Functor HtmlB a b
functor =
  { map = \f ->
    inj << Html.App.map f << prj
  }

semigroup : Semigroup (Html a)
semigroup =
  { concat = \x y ->
    div [] [x, y]
  }

monoid : Monoid (Html a)
monoid =
  { concat = semigroup.concat
  , empty = text ""
  }

inj : Html a -> App HtmlB a
inj =
  Brands.Unsafe.inj

prj : App HtmlB a -> Html a
prj =
  Brands.Unsafe.prj
