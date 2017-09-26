{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module ANF where

import Fresh
import AST
import Data.List (transpose)

isValue :: Expr -> Bool
isValue e0 =
  case e0 of
    Lambda{} -> True
    Function{} -> True
    Name{} -> True
    Lit{} -> True
    Bin{} -> True
    Apply Bin{} vs -> all isValue vs
    Apply (Name f) vs -> f `elem` [wired "puts", wired "show"] && all isValue vs
    _ -> False

anf :: Expr -> Fresh Expr
anf e0 =
  case e0 of
    Function mn tvs ps mt rhs -> Function mn tvs ps mt <$> anf rhs
    Lambda ps e -> Lambda ps <$> anf e
    Lit l  -> return (Lit l)
    Bin b  -> return (Bin b)
    Name n -> return (Name n)
    Switch es cases ->
      do (ds,xs) <-
           unzip <$>
             sequence
               [ let' "x" =<< anf (e `Labels` [c | ConP c _ <- pats])
               | (e,pats) <- es `zip` transpose [ps | Case ps _ <- cases]
               ]
         decls (concat ds) . Switch xs <$>
           sequence
             [ Case ps <$> anf rhs
             | Case ps rhs <- cases
             ]
    Apply e es ->
      do (d,f) <- let' "f" =<< anf e
         (ds,xs) <- unzip <$> sequence [ let' "x" =<< anf e | e <- es ]
         return (decls (concat (d:ds)) (Apply f xs))
    Seq (Let p e) b ->
      do l <- Let p <$> anf e
         Seq l <$> anf b
    Seq d b -> Seq d <$> anf b
    e `Sig` t -> (`Sig` t) <$> anf e
    e `Labels` ls -> (`Labels` ls) <$> anf e
    e `TyApply` ts -> (`TyApply` ts) <$> anf e

let' :: String -> Expr -> Fresh ([Decl], Expr)
let' hint e0
  | isValue e0 = return ([], e0)
  | otherwise =
      do x <- fresh hint
         return ([Let (NameP x) e0], Name x)

