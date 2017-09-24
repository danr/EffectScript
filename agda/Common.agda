module Common where

open import Data.Nat
open import Data.List hiding (all)

data _∈_ {A : Set} : A → List A → Set where
  here : ∀ {x xs} → x ∈ (x ∷ xs)
  there : ∀ {x y xs} → x ∈ xs → x ∈ (y ∷ xs)

rest : ∀ {A : Set} {x : A} {xs : List A} → x ∈ xs → List A
rest {xs = _ ∷ xs} here = xs
rest {xs = x ∷ _} (there pf) = x ∷ rest pf

postulate
  LabelNumber : Set
  Effect : Set

module Label (Type : Set) where
  record Label : Set where
    inductive
    constructor Lbl
    field
      label : LabelNumber
      eff : Effect
      arg : Type
      res : Type

Effects = List Effect

module Vars (Type : Set) where
  Context : Set
  Context = List Type

  data Var : Context → Type → Set where
    zero : ∀ {T Ts} → Var (T ∷ Ts) T
    succ : ∀ {T U Ts} → Var Ts U → Var (T ∷ Ts) U
