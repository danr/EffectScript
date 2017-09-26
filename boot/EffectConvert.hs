{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module EffectConvert where

import Control.Monad.State
import Control.Monad
import AST
import Fresh
import ANF (isValue)
import Data.List (find)

newtype EC a = EC (StateT [Name] Fresh a)
  deriving (Monad, Applicative, Functor, MonadState [Name])

runEC :: EC a -> Fresh a
runEC (EC m) = evalStateT m []

isOp :: Expr -> EC (Maybe Name)
isOp (Name op) = gets (find (op ==))
isOp _ = return Nothing

registerOps :: [Name] -> EC ()
registerOps = modify . (++)

instance MonadFresh EC where
  fresh = EC . lift . fresh

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

    Effect head cons `Seq` e2 ->
      do registerOps [ op | Con op _ _ _ <- cons ]
         (Algebraic head (Con doneName [] [] Nothing:cons) `Seq`) <$> ec e2 h k

    _ `Seq` e2 -> ec e2 h k

    e `Sig` _ -> ec e h k
    TyApply e _ -> ec e h k

    Switch vs cases ->
      do Switch vs <$>
           sequence
             [ Case ps <$> ec rhs h k
             | Case ps rhs <- cases
             ]

    v | isValue v -> return (k `Apply` [h, v])

    Apply (Name next) [p] `Labels` labels
      | next == nextName ->
      do vp <- ecVal p
         let h0 = h
         let k0 = k
         let label_repr:_ = labels
         h <- fresh "h"
         x <- fresh "x"
         k <- fresh "k"
         return $ Let (NameP k) k0 `Seq`
           (vp `Apply`
             [ addHandler $$ (h0:Name k:[ Quote label | label <- labels, label /= doneName ])
             , lam [h, x]
                 (k $$ [removeHandler $$ [Name h, Quote label_repr],
                        doneName $$ [Name x]])
             ])

    Apply f xs ->
      do vs <- mapM ecVal xs
         m_op <- isOp f
         case m_op of
           Just op ->
             do h' <- fresh "h"
                y <- fresh "y"
                k' <- fresh "k"
                return $
                  (getHandler $$ [h, Quote op]) `Apply` [
                    removeHandler $$ [h, Quote op],
                    op $$ (xs ++ [Lambda [NameP y, NameP h', NameP k']
                                         (k `Apply` [Name h', (k' $$ [Name h', Name y])])])
                                        -- (k `Apply` [Name h', Name y])])
                  ]
           Nothing -> return (f `Apply` (vs ++ [h,k]))

    e `Labels` _ -> ec e h k

f $$ xs = Apply (Name f) xs
lam ns rhs = Lambda (map NameP ns) rhs

