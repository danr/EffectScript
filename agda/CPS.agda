module CPS where

open import Data.List hiding (drop)
open import Common
open import Relation.Binary.PropositionalEquality hiding (trans)
import Effects as E

infixr 20 _⇒_

data Type : Set where
  void : Type
  unit : Type
  _⇒_ : Type → Type → Type
  -- The magic effect handlers type.
  handlers : Effects → Type
  -- The effect data for a particular label.
  ldata : ∀ l {effs} → Label.Label.eff {Type} l ∈ effs → Type
  -- The effect data for any label from a given effect.
  -- Essentially an existentially-quantified ldata.
  effdata : ∀ {eff : Effect} {effs : Effects} → eff ∈ effs → Type

open Label Type
open Vars Type

-- The type of the normal return continuation.
cont : Type → Effects -> Type
cont T effs = handlers effs ⇒ T ⇒ void

-- An effectful computation.
⟦_∣_⟧ : Type → Effects → Type
⟦ T ∣ effs ⟧ = handlers effs ⇒ cont T effs ⇒ void

-- An effectful function.
⟦_⇒_!_⟧ : Type → Type → Effects → Type
⟦ T ⇒ U ! effs ⟧ = T ⇒ ⟦ U ∣ effs ⟧

-- A handler for a single effect.
handler : ∀ {effs} l → Label.eff l ∈ effs → Type
handler l pf = cont (ldata l pf) (rest pf)

infixl 50 _$_

data Expr : Context → Type → Set where
  -- Normal lambda-calculus stuff.
  var : ∀ {Γ T}
    (x : Var Γ T) →
    Expr Γ T
  Λ_ : ∀ {Γ T U}
    (body : Expr (T ∷ Γ) U) →
    Expr Γ (T ⇒ U)
  _$_ : ∀ {Γ T U}
    (f : Expr Γ (T ⇒ U)) →
    (x : Expr Γ T) →
    Expr Γ U
  lett : ∀ {Γ T U}
    (e1 : Expr Γ T) →
    (e2 : Expr (T ∷ Γ) U) →
    Expr Γ U
  unit : ∀ {Γ} →
    Expr Γ unit

  -- The ldata constructor.
  -- An ldata is a pair of the yielded value and the resumption
  -- continuation.
  make-ldata : ∀ {Γ effs}
    (l : Label) →
    (pf : Label.eff l ∈ effs) →
    (x : Expr Γ (Label.arg l)) →
    (k : Expr Γ ⟦ Label.res l ⇒ void ! effs ⟧) →
    Expr Γ (ldata l pf)

  -- Match on an ldata.
  match-ldata : ∀ {Γ effs T}
    (l : Label) →
    (pf : Label.eff l ∈ effs) →
    (x : Expr Γ (ldata l pf)) →
    (k : Expr (Label.arg l ∷ ⟦ Label.res l ⇒ void ! effs ⟧ ∷ Γ) T) →
    Expr Γ T

  -- The effdata constructor.
  -- Turns an ldata into an effdata.
  make-effdata : ∀ {Γ effs}
    (l : Label) →
    (pf : Label.eff l ∈ effs) →
    Expr Γ (ldata l pf) →
    Expr Γ (effdata pf)

  -- Match an effdata against a particular label.
  match-effdata : ∀ {Γ T effs}
    (l : Label) → -- expected label
    (pf : Label.eff l ∈ effs) →
    Expr Γ (effdata pf) →
    Expr (ldata l pf ∷ Γ) T → -- success branch
    Expr Γ T → -- failure branch
    Expr Γ T

  -- Call an effect handler.
  get-handler : ∀ {Γ effs}
    (l : Label) →
    (pf : Label.eff l ∈ effs) →
    (h : Expr Γ (handlers effs)) →
    Expr Γ (handler l pf)

  -- Add an effect handler.
  add-handler : ∀ {Γ eff effs}
    (pf : eff ∈ effs) →
    -- We must handle every label of this effect.
    (∀ (l : Label) →
       (eq : eff ≡ Label.eff l) →
      Expr Γ (handler l (subst (λ eff → eff ∈ effs) eq pf))) →
    Expr Γ (handlers (rest pf)) →
    Expr Γ (handlers effs)

  -- Remove an effect handler.
  remove-handler : ∀ {Γ eff effs}
    (pf : eff ∈ effs) →
    Expr Γ (handlers effs) →
    Expr Γ (handlers (rest pf))

-- Embedding one context within another.
data _⊆_ : Context → Context → Set where
  id : ∀ {Γ} → Γ ⊆ Γ
  keep : ∀ {T Γ Δ} → Γ ⊆ Δ → (T ∷ Γ) ⊆ (T ∷ Δ)
  drop : ∀ {T Γ Δ} → Γ ⊆ Δ → Γ ⊆ (T ∷ Δ)
  trans : ∀ {Γ Δ Ε} → Γ ⊆ Δ → Δ ⊆ Ε → Γ ⊆ Ε

instance
  idInst : ∀ {Γ} → Γ ⊆ Γ
  idInst = id
  keepInst : ∀ {T Γ Δ} → {{inst : Γ ⊆ Δ}} → (T ∷ Γ) ⊆ (T ∷ Δ)
  keepInst {{inst}} = keep inst
  dropInst : ∀ {T Γ Δ} → {{inst : Γ ⊆ Δ}} → Γ ⊆ (T ∷ Δ)
  dropInst {{inst}} = drop inst

inj-var : ∀ {Γ Δ T} → {{_ : Γ ⊆ Δ}} → Var Γ T → Var Δ T
inj-var {{id}} x = x
inj-var {{keep _}} zero = zero
inj-var {{keep rel}} (succ x) = succ (inj-var {{rel}} x)
inj-var {{drop rel}} x = succ (inj-var {{rel}} x)
inj-var {{trans rel1 rel2}} x = inj-var {{rel2}} (inj-var {{rel1}} x)

inj : ∀ {T Γ Δ} → {{_ : Γ ⊆ Δ}} → Expr Γ T → Expr Δ T
inj (var x) = var (inj-var x)
inj (Λ x) = Λ (inj x)
inj (e1 $ e2) = inj e1 $ inj e2
inj (lett e1 e2) = lett (inj e1) (inj e2)
inj unit = unit
inj (make-ldata l pf x k) = make-ldata l pf (inj x) (inj k)
inj (match-ldata l pf x k) = match-ldata l pf (inj x) (inj k)
inj (get-handler l pf h) = get-handler l pf (inj h)
inj (add-handler pf es e) = add-handler pf (λ l eq → inj (es l eq)) (inj e)
inj (remove-handler pf e) = remove-handler pf (inj e)
inj (make-effdata l pf e) = make-effdata l pf (inj e)
inj (match-effdata l pf e1 e2 e3) = match-effdata l pf (inj e1) (inj e2) (inj e3)

-- Handy-dandy variables.
v0 : ∀ {Γ T} → Expr (T ∷ Γ) T
v0 = var zero

v1 : ∀ {Γ T U} → Expr (T ∷ U ∷ Γ) U
v1 = inj {{drop id}} v0

v2 : ∀ {Γ T U V} → Expr (T ∷ U ∷ V ∷ Γ) V
v2 = inj {{drop id}} v1

v3 : ∀ {Γ T U V W} → Expr (T ∷ U ∷ V ∷ W ∷ Γ) W
v3 = inj {{drop id}} v2

-- Translating types.
⟦_⟧τ : E.Type → Type
⟦ E.void ⟧τ = void
⟦ E.unit ⟧τ = unit
⟦ T E.⇒ U ! effs ⟧τ =
  ⟦ T ⟧τ ⇒ ⟦ ⟦ U ⟧τ ∣ effs ⟧
⟦ E.effdata pf ⟧τ = effdata pf

⟦_⟧γ : E.Context → Context
⟦ [] ⟧γ = []
⟦ T ∷ Ts ⟧γ = ⟦ T ⟧τ ∷ ⟦ Ts ⟧γ

⟦_⟧v : ∀ {Γ T} → E.Var Γ T → Var ⟦ Γ ⟧γ ⟦ T ⟧τ
⟦ E.zero ⟧v = zero
⟦ E.succ x ⟧v = succ ⟦ x ⟧v

⟦_⟧ℓ : E.Label → Label
⟦ Common.Label.Lbl label eff arg res ⟧ℓ =
  Lbl label eff ⟦ arg ⟧τ ⟦ res ⟧τ

-- Translating values and expressions.
mutual
  ⟦_⟧V : ∀ {Γ Δ T} →
    {{_ : ⟦ Γ ⟧γ ⊆ Δ}} →
    E.Value Γ T →
    Expr Δ ⟦ T ⟧τ
  ⟦ E.var x ⟧V =
    inj (var ⟦ x ⟧v)
  ⟦ E.lam body ⟧V =
    -- \x h k -> body h k (where h may be bound in body)
    Λ Λ Λ ⟦ body ⟧ v1 v0
  
  ⟦_⟧ : ∀ {Γ Δ T effs} →
    {{ _ : ⟦ Γ ⟧γ ⊆ Δ }} →
    E.Expr Γ T effs →
    Expr Δ (handlers effs) →
    Expr Δ (cont ⟦ T ⟧τ effs) →
    Expr Δ void
  ⟦ E.val x ⟧ h k =
    k $ h $ ⟦ x ⟧V
  ⟦ E.app f x ⟧ h k =
    ⟦ f ⟧V $ ⟦ x ⟧V $ h $ k
  ⟦ E.lett e1 e2 ⟧ h k =
    -- e1 h (\h' x -> e2 h' k) (where x may be bound in e2)
    ⟦ e1 ⟧ h (Λ Λ ⟦ e2 ⟧ v1 (inj k))
  ⟦ E.do {l = l} pf x ⟧ h k =
    -- get-handler h l
    --   (remove-handler eff h)
    --   l(x, \y h' _ -> k h' y)
    get-handler ⟦ l ⟧ℓ pf h $
      remove-handler pf h $
      make-ldata ⟦ l ⟧ℓ pf ⟦ x ⟧V
        (Λ Λ Λ inj k $ v1 $ v2)
  ⟦ E.case {l = l} pf x e1 e2 ⟧ h k =
    -- case x of
    --   effdata(l(y, k1)) -> e1[y, k1]
    --   _ -> e2
    -- Note: k and h are passed through to ⟦ e1 ⟧ and ⟦ e2 ⟧ unchanged
    match-effdata ⟦ l ⟧ℓ pf (inj (var ⟦ x ⟧v))
      (match-ldata ⟦ l ⟧ℓ pf v0 (⟦ e1 ⟧ (inj h) (inj k)))
      (⟦ e2 ⟧ h k)
  ⟦ E.next {eff = eff} pf x ⟧ h k =
    -- x ()
    --   (add-handler eff
    --     [ \h ldata -> k h effdata(ldata)
    --     | l <- labels eff ]
    --     (error "returned from next")
    ⟦ x ⟧V $ unit $
      add-handler pf
        (λ l eq → makeHandler pf l eq k) h $
      -- v0 here has type void, i.e., this calls error
      (Λ Λ v0)

  makeHandler  :
    ∀ {Γ eff effs} (pf : eff ∈ effs) l (eq : eff ≡ Label.eff l) →
    (k : Expr Γ (cont (effdata pf) (rest pf))) →
    Expr Γ (handler l (subst (λ eff → eff ∈ effs) eq pf))
  makeHandler pf l refl k =
    Λ Λ inj k $ v1 $ make-effdata l pf v0
      
    
