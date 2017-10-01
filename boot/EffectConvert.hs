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
    Let ps (Apply (Name local) [e1]) `Seq` e2
      | local == localName ->
         do ec e1 h . Lambda [Wild, ps] =<< ec e2 h k

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

    v | isValue v -> return (k `Apply` [h, v])

    Op{} -> error "unapplied op"

    Switch Nothing vs cases ->
      do Switch Nothing vs <$>
           sequence
             [ Case ps <$> ec rhs h k
             | Case ps rhs <- cases
             ]

{-

    [[handle vp, v1, v2 {
        case op(x1, x2), k, s1, s2: P
        case done(r), _, s1, s2: Q
      }|h0|k0]]


=>

    function loop(h, x, v1, v2) {
      switch x, v1, v2 {
        case pair(op(x1, x2), pk), s1, s2:
          let pk0 = pk
          let pk = (y, t1, t2, h, k) => k(h, pk0(h + {op:(h, x) => loop(h, x, t1, t2)}, y))
          in [[P|h-op|k0]]
        case pair(done(r), _), s1, s2:
          [[Q|h-op|k0]]
      }
    }

    vp(h0 + {op:(h, x) => loop(h, x, v1, v2)},
       (_h, x) => absurd x

-}

{-
    Switch (Just (p `Labels` labels)) vs cases ->
      do vp <- ecVal p
         loop <- fresh "loop"
         x <- fresh "x"
         ss <- sequence [ fresh "s" | _ <- vs ]
         h' <- fresh "h"
         let label_repr:_ = labels
         cases <- sequence
           [ do rhs' <- ec rhs (removeHandler $$ [Name h', Quote label_repr]) k
                y <- fresh "y"
                ts <- sequence [ fresh "t" | _ <- vs ]
                hw <- fresh "hw"
                k <- fresh "k"
                h'' <- fresh "h"
                x' <- fresh "x"
                w <- fresh "kw"
                case p_k of
                  NameP pk ->
                    do let p_k_lam =
                             lam
                               ((y:ts) ++ [hw,w])
                               (k $$
                                   [Name y,
                                    addHandler $$
                                     (Name h'
                                     :lam [h'', x'] (loop $$ (map Name ([h'', x'] ++ ts)))
                                     :[ Quote label | label <- labels, label /= doneName ])
                                   , Name w])
                       return $ Case
                         (ConP pairName [p_op,p_k]:ps)
                         (inlineLet (NameP k) (Name pk) (inlineLet p_k p_k_lam rhs'))
                  Wild ->
                    return $ Case (ConP pairName [p_op,p_k]:ps) rhs'

           | Case (p_op:p_k:ps) rhs <- cases
           ]
         return $ Let (NameP loop)
             (Function (Just loop) [] (map (Without . NameP) (h':x:ss)) Nothing
                (Switch Nothing (Name x:map Name ss) cases))
            `Seq` (vp `Apply` [ addHandler $$
                                 (h:
                                  lam [h', x] (loop $$ (map Name [h',x] ++ vs)):
                                  [ Quote label | label <- labels, label /= doneName ])
                              , lam [h', x] (Lit (String "Void") `Apply` [Name x])
                              ])
                              -}


    Apply (Name next `Labels` labels) [p]
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
         return $
           (getHandler $$ [h, Quote op]) `Apply` [
             h,
               DataCon op `Apply` (xs++[
                Lambda [NameP y, NameP h', Wild]
                             (k `Apply` [Name h', Name y])])
           ]
    Apply f xs ->
      do vs <- mapM ecVal xs
         return (f `Apply` (vs ++ [h,k]))

    e `Labels` _ -> ec e h k

