{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ViewPatterns #-}
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

nil :: Expr -> Expr -> Expr
nil h t = wired "nil" `Apply` [h,t]

cons :: Expr -> Expr -> Expr
cons h t = wired "cons" `Apply` [h,t]

uncons :: Expr -> (Expr, Expr)
uncons e = (hHead e, hTail e)

hHead :: Expr -> Expr
hHead (wired "cons" `Apply` [h,_]) = h
hHead e = wired "head" `Apply` [e]

hTail :: Expr -> Expr
hTail (wired "tail" `Apply` [_,t]) = t
hTail e = wired "tail" `Apply` [e]

ecVal :: Expr -> EC Expr
ecVal v0 =
  case v0 of
    Function mn tvs ps rt rhs ->
      do ks <- fresh "ks"
         Function mn tvs (ps ++ [Without (NameP ks)]) rt <$> ec rhs (Name ks)

    Lambda ps rhs ->
      do ks <- fresh "ks"
         Lambda (ps ++ [Without (NameP ks)]) <$> ec rhs (Name ks)

    DataCon n vs -> DataCon n <$> mapM ecVal vs

    Lit{} -> return v0
    Bin{} -> return v0
    Name{} -> return v0
    Unrestricted{} -> error "unrestricted ecVal"

ec :: Expr -> Expr -> EC Expr
ec e0 ks0 =
  case e0 of
    Let ps e1 `Seq` e2 ->
      do let (k, ks) <- uncons ks0
         ks' <- fresh "ks"
         e2' <- ec e2 (k `cons` ks')
         ec e1 (Lambda [ps, NameP ks'] e2' `cons` ks)

    Effect{} `Seq` e2 -> ec e2 ks0
    Alias{} `Seq` e2 -> ec e2 ks0
    Algebraic{} `Seq` e2 -> ec e2 ks0

    e `Sig` _ -> ec e ks0
    TyApply e _ -> ec e ks0

    Done v ->
      do let (k, ks) <- uncons ks0
         v' <- ecVal v
         return (k `Apply` [v', ks])

    Op l vs ->
      do let (k, uncons -> (h, ks)) = uncons ks0
         vs' <- mapM ecVal vs
         return (h `Apply` [DataCon l (h `cons` (k `cons` nil) : vs), ks])

    Switch (Just m) vs0 cases ->
      do h_done <- -- continue here
         h_ops <- -- continue here
         m' <- ec m (h_done `cons` (h_ops `cons` ks0))

    Switch Nothing vs cases ->
      do vs' <- mapM ecVal vs
         Switch Nothing vs' <$>
           sequence
             [ Case ps <$> ec rhs ks0
             | Case ps rhs <- cases
             ]

