module Main exposing (main)

import Html exposing (..)
import Html.Attributes exposing (..)

import Brands.Functor exposing (set)
import Brands.Foldable exposing (foldMap)

import Brands.Html as BH
import Brands.List as BL
import Brands.Maybe as BM

main : Html a
main =
  div
    [ style
      [ ("display", "flex")
      , ("flex-direction", "column")
      ]
    ]
    [ span' <| BM.prj <| set BM.functor 12 <| BM.inj <| Nothing
    , span' <| BM.prj <| set BM.functor 12 <| BM.inj <| Just "hi"
    , foldMap BL.foldable BH.monoid viewLevel <| BL.inj userLevels
    ]

userLevels : List Level
userLevels =
  [ Administrator
  , User
  , User
  , Moderator
  , User
  ]

span' : a -> Html b
span' x =
  span [] [text <| toString x]

type Level
  = User
  | Moderator
  | Administrator

viewLevel : Level -> Html a
viewLevel level =
  case level of
    User ->
      text "Hello friend!"

    Moderator ->
      button [] [text "Mute everone"]

    Administrator ->
      button [] [text "Delete DB"]
