module Brands.Expr exposing (..)

import Brands.Leibniz exposing (..)

type Expr a
  = Val Int (Eq Int a)
  | Boolean Bool (Eq Bool a)
  | Add (Expr Int) (Expr Int) (Eq Int a)
  | Eq (Expr Int) (Expr Int) (Eq Bool a)
  | If (Expr Bool) (Expr a) (Expr a) (Eq a a)

val : Int -> Expr Int
val n =
  Val n refl

boolean : Bool -> Expr Bool
boolean b =
  Boolean b refl

add : Expr Int -> Expr Int -> Expr Int
add e1 e2 =
  Add e1 e2 refl

eq : Expr Int -> Expr Int -> Expr Bool
eq e1 e2 =
  Eq e1 e2 refl

if' : Expr Bool -> b -> Expr a -> b -> Expr a -> Expr a
if' b _ true _ false =
  If b true false refl

then' : ()
then' =
  ()

else' : ()
else' =
  ()

eval : Expr a -> a
eval expr =
  case expr of
    Val n prf ->
      cast prf n
    Boolean b prf ->
      cast prf b
    Add e1 e2 prf ->
      cast prf (eval e1 + eval e2)
    Eq e1 e2 prf ->
      cast prf (eval e1 == eval e2)
    If b true false prf ->
      cast prf (if eval b then eval true else eval false)

pretty : Expr a -> String
pretty expr =
  case expr of
    Val n prf ->
      toString (cast prf n)
    Boolean b prf ->
      toString (cast prf b)
    Add e1 e2 _ ->
      "(" ++ pretty e1 ++ " + " ++ pretty e2 ++ ")"
    Eq e1 e2 _ ->
      "(" ++ pretty e1 ++ " == " ++ pretty e2 ++ ")"
    If b true false _ ->
      "if " ++ pretty b ++ " then " ++ pretty true ++ " else " ++ pretty false
