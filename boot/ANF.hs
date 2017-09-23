{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module ANF where

import Fresh
import AST

anf :: Expr -> Fresh Expr
anf e0 =
  case e0 of
    Function mn tvs ps mt rhs -> Function mn tvs ps mt <$> anf rhs
    Lambda ps e -> Lambda ps <$> anf e
    Lit l  -> return (Lit l)
    Bin b  -> return (Bin b)
    Name n -> return (Name n)
    Switch es cs ->
      do (ds,xs) <-
           unzip <$>
             sequence
               [ let' "x" <$> anf (e `Labels` [c | ConP c _ <- pats])
               | (e,pats) <- es `zip` transpose [ps | Case ps _ <- cases]
               ]
         decls (concat ds) . Switch xs <$>
           sequence
             [ Case ps <$> anf rhs
             | Case ps rhs <- cases
             ]
    Apply e es ->
      do (d,f) <- let' "f" <$> anf e
         (ds,xs) <- unzip <$> sequence [ let' "x" <$> anf e | e <- es ]
         return (decls (concat (d:ds)) (Apply f xs))
    TyApply e ts ->
    Seq (Let p e) b ->
      do l <- Let p <$> anf e
         Seq l <$> anf b
    Seq d b -> Seq d <$> anf b
    e `Sig` t -> (`Sig` t) <$> anf e
    e `Labels` ls -> (`Labels` ls) <$> anf e

let x = 3
let y = x
in \ x . y

isValue :: Expr -> Bool
isValue e0 =
  case e0 of
    Lambda{} -> True
    Function{} -> True

let' :: String -> Expr -> Fresh ([Decl], Expr)
let' hint e0 =
  case e0 of
    Name n -> return ([], Name n)
    _ ->
      do x <- fresh hint
         return ([Let x e0], Name x)

