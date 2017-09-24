module Effects where

open import Data.List
open import Common

data Type : Set where
  void : Type
  unit : Type
  _⇒_!_ : Type → Type → Effects → Type
  effdata : {x : Effect} {xs : Effects} -> x ∈ xs → Type

open Label Type public
open Vars Type public

mutual
  data Expr : Context → Type → Effects → Set where
    val : ∀ {Γ effs T}
      (x : Value Γ T) →
      Expr Γ T effs
    app : ∀ {Γ effs T U}
      (f : Value Γ (T ⇒ U ! effs))
      (x : Value Γ T) →
      Expr Γ U effs
    do : ∀ {Γ l effs}
      (pf : Label.eff l ∈ effs) →
      (x : Value Γ (Label.arg l)) →
      Expr Γ (Label.res l) effs
    next : ∀ {Γ eff effs}
      (pf : eff ∈ effs) →
      (x : Value Γ (unit ⇒ void ! effs)) →
      Expr Γ (effdata pf) (rest pf)
    lett : ∀ {Γ effs T U}
      (e1 : Expr Γ T effs) →
      (e2 : Expr (T ∷ Γ) U effs) →
      Expr Γ U effs
    case : ∀ {Γ T effs l}
      (pf : Label.eff l ∈ effs) →
      (x : Var Γ (effdata pf)) →
      (e1 : Expr (Label.arg l ∷ (Label.res l ⇒ void ! effs) ∷ Γ) T effs) →
      (e2 : Expr Γ T effs) →
      Expr Γ T effs

  data Value : Context → Type → Set where
    var : ∀ {Γ T}
      (x : Var Γ T) →
      Value Γ T
    lam : ∀ {Γ T U effs} →
      (body : Expr (T ∷ Γ) U effs) →
      Value Γ (T ⇒ U ! effs)
