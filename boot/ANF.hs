{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
module ANF where

import Fresh
import AST
import Data.List (transpose)

anf :: Expr -> Fresh Expr
anf e0 =
  case e0 of
    Switch mh vs cases ->
      do mh' <- maybe (return Nothing) (fmap Just . anf) mh
         (ds,xs) <- unzip <$> sequence [ anfValue "x" v | v <- vs ]
         decls (concat ds) . Switch mh' xs <$>
           sequence
             [ Case ps <$> anf rhs
             | Case ps rhs <- cases
             ]
    Op l vs ->
      do (ds,xs) <- unzip <$> sequence [ anfValue "x" x | x <- vs ]
         return (decls (concat ds) (Op l xs))
    Apply v vs ->
      do (d,f) <- anfValue "f" v
         (ds,xs) <- unzip <$> sequence [ anfValue "x" x | x <- vs ]
         return (decls (concat (d:ds)) (apply f xs))
    Seq (Let p e) b ->
      do l <- Let p <$> anf e
         mkSeq l =<< anf b
    Seq d b -> mkSeq d =<< anf b
    Done v ->
      do (d,v') <- anfValue "v" v
         return (decls d (Done v'))
    e `Sig` t -> (`Sig` t) <$> anf e
    v `TyApply` ts ->
      do (d,vt) <- anfValue "t" v
         return (decls d (vt `TyApply` ts))
--    e `Labels` ls -> (`Labels` ls) <$> anf e

anfValue :: String -> Value -> Fresh ([Decl], Value)
anfValue hint =
  \ case
    Function mn tvs ps mt rhs -> ([],) . Function mn tvs ps mt <$> anf rhs
    Lambda ps e -> ([],) . Lambda ps <$> anf e
    Unrestricted e ->
      do x <- fresh hint
         e' <- anf e
         return ([Let (NameP x) e'], Name x)
    v -> return ([], v)

