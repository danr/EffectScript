{-# LANGUAGE TypeFamilies, KindSignatures, TypeOperators #-}
module TrAST where

import qualified AbsEffectScript as BNF
import qualified AST as AST

import AST (WithOptional(..))

class Tr a where
  type To a :: *
  tr :: a -> To a

instance Tr a => Tr [a] where
  type To [a] = [To a]
  tr = map tr

instance Tr BNF.Name where
  type To BNF.Name = AST.Name
  tr (BNF.Name (p,s)) = AST.MkName s p

instance Tr BNF.NameP where
  type To BNF.NameP = AST.Name
  tr (BNF.NameP (p,s)) = AST.MkName (init s) p

instance Tr BNF.NameK where
  type To BNF.NameK = AST.Name
  tr (BNF.NameK (p,s)) = AST.MkName (init s) p

instance Tr BNF.Program where
  type To BNF.Program = AST.Expr
  tr (BNF.Program ds) =
    trDecls (ds ++
      [BNF.Expr (BNF.Call (BNF.NameP ((0,0), "main(")) [])])

instance Tr BNF.Param where
  type To BNF.Param = AST.Pattern `WithOptional` AST.Type
  tr p = case p of
    BNF.Param e           -> Without (trPat e)
    BNF.ParamWithType e t -> trPat e `With` tr t


instance Tr BNF.Con where
  type To BNF.Con = AST.Con
  tr c = case c of
    BNF.Nul name optt         -> AST.Con (tr name) []      []      (tr optt)
    BNF.NulEx name ns optt    -> AST.Con (tr name) (tr ns) []      (tr optt)
    BNF.Con name ts optt      -> AST.Con (tr name) []      (tr ts) (tr optt)
    BNF.ConEx name ns ts optt -> AST.Con (tr name) (tr ns) (tr ts) (tr optt)

instance Tr BNF.Rhs where
  type To BNF.Rhs = AST.Expr
  tr r = case r of
    BNF.ExprRhs e  -> tr e
    BNF.DeclRhs ds -> trDecls ds

trDecls :: [BNF.Decl] -> AST.Expr
trDecls [] = error "Declarations not ending in an expression"
trDecls [d@(BNF.Expr BNF.Let{})] = trDecls [d, BNF.Expr BNF.Unit]
trDecls [BNF.Expr e] = tr e
trDecls (d:ds) = d' `AST.Seq` trDecls ds
  where
    d' = case d of
      BNF.Algebraic dh cs      -> AST.Algebraic (tr dh) (tr cs)
      BNF.Effect dh cs         -> AST.Effect (tr dh) (tr cs)
      BNF.Alias dh t           -> AST.Alias (tr dh) (tr t)
      BNF.Expr (BNF.Let e rhs) -> AST.Let (trPat e) (tr rhs)
      BNF.Expr e ->
        case tr e of
          fn@AST.Function{AST.fn_name=Just n} -> AST.Let (AST.NameP n) fn
          tr_e -> AST.Let AST.Wild tr_e

instance Tr BNF.Expr where
  type To BNF.Expr = AST.Expr
  tr e0 = case e0 of
    BNF.Unit                -> AST.Lit AST.Unit
    BNF.Function fh optt ds -> tr fh (tr optt) (trDecls ds)
    BNF.Lit lit             -> AST.Lit (tr lit)
    BNF.NameMono x          -> AST.Name (tr x)
    BNF.NamePoly x ts       -> AST.Name (tr x) `AST.TyApply` tr ts
    BNF.WildP               -> AST.Name AST.wildName
    BNF.Switch e cs         -> AST.Switch (tr (Commas e)) (tr cs)
    BNF.NSwitch cs          -> AST.Switch [] (tr cs)

    BNF.ApplyT e ts         -> tr e `AST.TyApply` tr ts
    BNF.Apply e es          -> foldl AST.Apply (tr e) (map (tr . Commas) es)
    BNF.Call f es           -> foldl AST.Apply (AST.Name (tr f)) (map (tr . Commas) es)

    BNF.CallTy f ts es      -> tr (BNF.NamePoly f ts `BNF.Apply` es)
    BNF.ApplyTE e ts es     -> tr (BNF.Apply (e `BNF.ApplyT` ts) es)

    BNF.Mul e1 e2           -> AST.Bin AST.Mul `AST.Apply` [tr e1, tr e2]
    BNF.Div e1 e2           -> AST.Bin AST.Div `AST.Apply` [tr e1, tr e2]
    BNF.Mod e1 e2           -> AST.Bin AST.Mod `AST.Apply` [tr e1, tr e2]
    BNF.Add e1 e2           -> AST.Bin AST.Add `AST.Apply` [tr e1, tr e2]
    BNF.Sub e1 e2           -> AST.Bin AST.Sub `AST.Apply` [tr e1, tr e2]
    BNF.Eq e1 e2            -> AST.Bin AST.Eq `AST.Apply` [tr e1, tr e2]
    BNF.Ne e1 e2            -> AST.Bin AST.Ne `AST.Apply` [tr e1, tr e2]
    BNF.Lt e1 e2            -> AST.Bin AST.Lt `AST.Apply` [tr e1, tr e2]
    BNF.Le e1 e2            -> AST.Bin AST.Le `AST.Apply` [tr e1, tr e2]
    BNF.Gt e1 e2            -> AST.Bin AST.Gt `AST.Apply` [tr e1, tr e2]
    BNF.Ge e1 e2            -> AST.Bin AST.Ge `AST.Apply` [tr e1, tr e2]

    BNF.Lambda e rhs        -> AST.Lambda (trPats e) (tr rhs)
    BNF.Let e rhs           -> error $ "Let in illegal position" ++ show e0
                               -- AST.Let (trPat e) (tr rhs)
    BNF.Signature e t       -> AST.Sig (tr e) (tr t)
    BNF.Lambda0 ds          -> AST.Lambda [] (trDecls ds)

    BNF.Comma e1 e2         -> error $ "Comma in illegal position" ++ show e0

newtype Commas = Commas BNF.Expr
  deriving (Eq, Ord, Show)

instance Tr Commas where
  type To Commas = [AST.Expr]
  tr = go 0
    where
    go :: Int -> Commas -> [AST.Expr]
    go 0 (Commas BNF.Unit) = []
    go i (Commas e) = case e of
      BNF.Comma e1 e2 -> go 1 (Commas e1) ++ go 1 (Commas e2)
      e -> [tr e]

trPat :: BNF.Expr -> AST.Pattern
trPat = pat . tr

trPats :: BNF.Expr -> [AST.Pattern]
trPats = map pat . tr . Commas

pat :: AST.Expr -> AST.Pattern
pat e = case e of
  AST.Lit lit               -> AST.LitP lit
  AST.Name x
    | x == AST.wildName     -> AST.Wild
    | otherwise             -> AST.NameP x
  AST.Apply (AST.Name k) es -> AST.ConP k (map pat es)
  e -> error $ "Not a pattern: " ++ show e

instance Tr BNF.Lit where
  type To BNF.Lit = AST.Lit
  tr l = case l of
    BNF.Integer x -> AST.Integer x
    BNF.String s  -> AST.String s

instance Tr BNF.Case where
  type To BNF.Case = AST.Case
  tr (BNF.Case e ds) = AST.Case (trPats e) (trDecls ds)

instance Tr BNF.Type where
  type To BNF.Type = AST.Type
  tr = top
    where
    top t0 = case t0 of
      BNF.Bang{}     -> go (BNF.UnitTy `BNF.ArrTy` t0)
      BNF.BangOnly{} -> go (BNF.UnitTy `BNF.ArrTy` BNF.Bang BNF.UnitTy t0)
      _              -> go t0

    go t0 = case t0 of
      BNF.NameTy name   -> AST.NameTy (tr name)
      BNF.AppTy name ts -> AST.TyCon (tr name) (tr ts)
      BNF.UnitTy        -> AST.TyCon AST.unitTyConName []

      BNF.ArrTys ts (BNF.Bang r e) ->
        let AST.Arrow ts' r' e' = go (BNF.ArrTys ts r)
        in  AST.Arrow ts' r' (e' ++ [top e])

      BNF.ArrTys ts (BNF.BangOnly e) -> go (BNF.ArrTys ts (BNF.UnitTy `BNF.Bang` e))
      BNF.ArrTys ts t -> AST.Arrow (map top ts) (top t) []

      BNF.ArrTy t1 t2 -> go (BNF.ArrTys [t1] t2)

      BNF.Bang{}     -> error "internal error: call top in Tr BNF.Type"
      BNF.BangOnly{} -> error "internal error: call top in Tr BNF.Type"

instance Tr BNF.OptType where
  type To BNF.OptType = Maybe AST.Type
  tr ot = case ot of
    BNF.NoType -> Nothing
    BNF.Type t -> Just (tr t)

instance Tr BNF.DeclHead where
  type To BNF.DeclHead = AST.TypeHead
  tr dh = case dh of
    BNF.MonoHead x    -> AST.TypeHead (tr x) []
    BNF.PolyHead x ns -> AST.TypeHead (tr x) (tr ns)

instance Tr BNF.FunctionHead where
  type To BNF.FunctionHead = Maybe AST.Type -> AST.Expr -> AST.Expr
  tr fh = case fh of
    BNF.Anonymous ps              -> AST.Function Nothing       []       (tr ps)
    BNF.MonoFunctionHead x ps     -> AST.Function (Just (tr x)) []       (tr ps)
    BNF.PolyFunctionHead x tvs ps -> AST.Function (Just (tr x)) (tr tvs) (tr ps)

instance Tr BNF.TypeParam where
  type To BNF.TypeParam = AST.Type `WithOptional` AST.Name
  tr tp = case tp of
    BNF.OnlyType t      -> Without (tr t)
    BNF.WithName x t    -> tr t `With` tr x

