{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module EffectConvert where

import Control.Monad.State
import Control.Monad
import AST
import Fresh

isValue :: Expr -> Bool
isValue e0 =
  case e0 of
    Lambda{} -> True
    Function{} -> True
    Name{} -> True
    Lit{} -> True
    Bin{} -> True
    Apply Bin{} vs -> all isValue vs
    _ -> False

ec :: Expr -> Expr -> Expr -> EC Expr
ec e0 h k =
  case e0 of
    Let ps v1 `Seq` e2 | isValue v1
      do Let ps v1 <$> ec e2 h k

    Let ps e1 `Seq` e2 ->
      do h1 <- Name <$> fresh "h"
         ec e1 h . Lambda (h1:ps) <$> ec e2 h1 k

    d `Seq` e2 ->
      do case d of
           Effect _ cons -> modify ([ op | Con op _ _ _ <- cons ] ++)
           _ -> return ()
         ec e2 h k

    e `Sig` _ -> ec e h k
    TyApply e _ -> ec e h k

    Switch vs cases ->
      do Switch vs <$>
           sequence
             [ Case ps <$> ec rhs h k
             | Case ps rhs <- cases
             ]

    v | isValue v -> return v

    Apply (Name next) [p] `Labels` labels
      | next == nextName ->
      do let h0 = h
         let k0 = k
         v <- fresh "v"
         h <- fresh "h"
         k <- fresh "k"
         x <- fresh "x"
         wild <- fresh "wild"
         let f $$ xs = Apply (Name f) xs
         let lam ns rhs = Lambda (map NameP ns) rhs
         let lamC l ns rhs = Lambda (ConP l (map NameP ns)) rhs
         return $ p `Apply`
           [ Lit Unit
           , addHandlers $$ (h0:
              [ lamC l
                  [v, h, k]
                  (k0 `Apply` [removeHandlers $$ (h:labels),
                               l $$ [Name v,
                                     (lam [x, h, wild]
                                          (k $$ [Name h, Name x]))]])
              | l <- labels
              ])
           , lam [h, x] (h `Apply` [doneName $$ [Name x, Name h, Name k0]])
           ]

    Apply f xs ->
      op <- isOp f
      if op then
        return (h `Apply` (f `Apply` (xs ++ [h,k])))
      else
        return (f `Apply` (xs ++ [h,k]))

    e `Labels` _ -> ec e h k


isOp :: Name -> EC Bool
isOp = gets . elem

