module Main exposing (main)

import Html exposing (..)
import Html.Attributes exposing (..)

import Brands.Functor exposing (set)
import Brands.Foldable exposing (foldMap)

import Brands.Expr exposing (..)
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
    , fieldset
      []
      [ legend [] [text "Expr a"]
      , section
        []
        [ h4 [] [text "Pretty print"]
        , article
          []
          [ span [] [text "three: "]
          , span [] [text (pretty three)]
          ]
        , article
          []
          [ span [] [text "four: "]
          , span [] [text (pretty four)]
          ]
        , article
          []
          [ span [] [text "conditional: "]
          , span [] [text (pretty conditional)]
          ]
        ]
      , section
        []
        [ h4 [] [text "Evaluated"]
        , article
          []
          [ span [] [text "three: "]
          , span [] [text (toString (eval three))]
          ]
        , article
          []
          [ span [] [text "four: "]
          , span [] [text (toString (eval four))]
          ]
        , article
          []
          [ span [] [text "conditional: "]
          , span [] [text (toString (eval conditional))]
          ]
        ]
      ]
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

three : Expr Int
three =
  add (val 1) (val 2)

four : Expr Int
four =
  add (val 1) three

conditional : Expr Int
conditional =
  if' (eq three four) then'
    three
  else'
    four
