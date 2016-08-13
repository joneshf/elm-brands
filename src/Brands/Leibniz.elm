module Brands.Leibniz exposing (..)

import Brands exposing (..)
import Brands.Id as Id exposing (..)
import Brands.Embed as Embed exposing (..)
import Brands.Flip as Flip exposing (..)
import Brands.Unsafe exposing (..)

type Leibniz f a b =
  Leibniz (App f a -> App f b)

runLeibniz : Leibniz f a b -> App f a -> App f b
runLeibniz (Leibniz f) =
  f

type EqB
  = EqB

type alias Eq a b =
  App (App EqB a) b

inj : Leibniz f a b -> Eq a b
inj =
  Brands.Unsafe.inj

prj : Eq a b -> Leibniz f a b
prj =
  Brands.Unsafe.prj

refl : Eq a a
refl =
  inj (Leibniz identity)

symm : Eq a b -> Eq b a
symm eq =
  runFlip (Flip.prj (subst eq (Flip.inj (Flip refl))))

trans : Eq a b -> Eq b c -> Eq a c
trans ab bc =
  subst bc ab

cast : Eq a b -> a -> b
cast eq a =
  Id.prj (runLeibniz (prj eq) (Id.inj a))

subst : Eq a b -> App f a -> App f b
subst =
  runLeibniz << prj

cong : Eq a b -> Eq (App f a) (App f b)
cong eq =
  inj (Leibniz (Embed.embed >> Embed.prj >> (Embed.run << Embed.inj << subst eq)))
