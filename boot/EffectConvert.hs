{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module EffectConvert where

import Control.Monad.State
import Control.Monad
import AST
import Fresh
import ANF (isValue)
import Data.List (find)

newtype EC a = EC (Fresh a)
  deriving (Monad, Applicative, Functor, MonadFresh)

runEC :: EC a -> Fresh a
runEC (EC m) = m

ecVal :: Expr -> EC Expr
ecVal e0 =
  case e0 of
    Function mn tvs ps rt rhs ->
      do h <- fresh "h"
         k <- fresh "k"
         Function mn tvs (ps ++ map (Without . NameP) [h, k]) rt <$> ec rhs (Name h) (Name k)

    Lambda ps rhs ->
      do h <- fresh "h"
         k <- fresh "k"
         Lambda (ps ++ map NameP [h, k]) <$> ec rhs (Name h) (Name k)

    _ -> return e0


ec :: Expr -> Expr -> Expr -> EC Expr
ec e0 h k =
  case e0 of
    Let ps v1 `Seq` e2 | isValue v1 ->
      do v' <- ecVal v1
         (Let ps v' `Seq`) <$> ec e2 h k

    Let ps e1 `Seq` e2 ->
      do h1 <- fresh "h"
         ec e1 h . Lambda [NameP h1, ps] =<< ec e2 (Name h1) k

    Effect{} `Seq` e2 -> ec e2 h k
    Alias{} `Seq` e2 -> ec e2 h k
    Algebraic{} `Seq` e2 -> ec e2 h k

    e `Sig` _ -> ec e h k
    TyApply e _ -> ec e h k

    Switch vs cases ->
      do Switch vs <$>
           sequence
             [ Case ps <$> ec rhs h k
             | Case ps rhs <- cases
             ]

    v | isValue v -> return (k `Apply` [h, v])

    Op{} -> error "unapplied op"

    Apply (Name next) [p] `Labels` labels
      | next == nextName ->
      do let label_repr:_ = labels
         vp <- ecVal p
         let h0 = h
         let k0 = k
         k0' <- do h <- fresh "h"
                   x <- fresh "x"
                   return $ lam [h, x] $ k0 `Apply` [removeHandler $$ [Name h, Quote label_repr], Name x]
         h <- fresh "h"
         x <- fresh "x"
         k <- fresh "k"
         return $
           (vp `Apply`
             [ addHandler $$
                (h0:
                 k0':
                 [ Quote label | label <- labels, label /= doneName ])
             , lam [h, x] (Lit (String "Void") `Apply` [Name x])
             ])

    Apply (Op op) xs ->
      do y <- fresh "y"
         h' <- fresh "h"
         k' <- fresh "k"
         return $
           (getHandler $$ [h, Quote op]) `Apply` [
             h,
             DataCon op `Apply`
                (xs ++ [Lambda [NameP y, NameP h', NameP k']
                        (k `Apply` [Name h', Name y])])
           ]
    Apply f xs ->
      do vs <- mapM ecVal xs
         return (f `Apply` (vs ++ [h,k]))

    e `Labels` _ -> ec e h k

f $$ xs = Apply (Name f) xs
lam ns rhs = Lambda (map NameP ns) rhs

