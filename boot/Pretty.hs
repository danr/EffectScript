{-# LANGUAGE OverloadedStrings, TypeOperators #-}
module Pretty where

import AST
import Text.PrettyPrint
import Data.List (intersperse)

($\) :: Doc -> Doc -> Doc
d $\ d2 = cat [d, nest 2 d2]

infixr 2 $\

embrace :: Doc -> Doc -> Doc
embrace h b = ((h <+> "{") $$ nest 2 b) $$ "}"

emparens :: Doc -> [Doc] -> Doc
emparens h bs = (h <> "(") $\ csv bs <> ")"

csv :: [Doc] -> Doc
csv = fsep . punctuate ","

crocodile :: PP a => [a] -> Doc
crocodile [] = empty
crocodile xs = "<" <> csv (map pp xs) <> ">"

class PP a where
  pp :: a -> Doc

class PPrec a where
  ppr :: Int -> a -> Doc

instance PP Name where
  pp (MkName n _) = text n

pretty :: PP a => a -> String
pretty = render . pp

instance PP TypeHead where
  pp th = case th of
    TypeHead t [] -> pp t
    TypeHead t ts -> pp t <> crocodile ts

instance PP Decl where
  pp d = case d of
    Algebraic th cs -> embrace ("data" <+> vcat (map pp cs)) (vcat (map pp cs))
    Effect th cs -> embrace ("effect" <+> vcat (map pp cs)) (vcat (map pp cs))
    Alias th t -> "alias" <+> pp th <+> "=" $\ pp t
    Expr e -> pp e

instance PP Con where
  pp (Con x ts as mrt) =
    pp x <> crocodile ts
         <> parens (csv (map pp as))
         <> mrt ? (":" $\)

(?) :: PP a => Maybe a -> (Doc -> Doc) -> Doc
Just a  ? f = f (pp a)
Nothing ? _ = empty

infix 9 ?

instance (PP a, PP b) => PP (a `WithOptional` b) where
  pp (a `With` b) = pp a <> ":" $\ pp b
  pp (Without a) = pp a

instance PP Expr where pp = ppr 0
instance PPrec Expr where
  ppr i e0 = parIf (prec e0 <= i) $ case e0 of
    Function mf tvs ps mrt e ->
      ("function" $\ emparens (mf ? (" " <>) <> crocodile tvs) (map pp ps))
      `embrace` pp e
    Lambda [] e -> embrace empty (pp e)
    Lambda ps (Decls ds) -> embrace (parens (csv (map pp ps)) $\ "=>") (vcat (map pp ds))
    Lambda ps e -> parIf (i > 12) (csv (map pp ps)) <+> "=>" $\ ppr 4 e
    Let p e -> pp p <+> "=" $\ ppr 4 e
    Lit l -> pp l
    Apply (Bin b) [e1,e2] -> ppr (lprec b) e1 <+> pp b <+> ppr (rprec b) e2 -- todo: precedences
    Name n -> pp n
    Switch es cs -> ("switch" $\ csv (map pp es)) `embrace` (vcat (map pp cs))
    Apply e es -> emparens (pp e) (map pp es)
    TyApply e ts -> ppr 15 e <> crocodile ts
    Decls ds -> vcat (map pp ds)
    Sig e t -> ppr 3 e <> ":" $\ pp t

prec :: Expr -> Int
prec e = case e of
  Decls{}    -> 15
  Sig{}      -> 2
  Lambda{}   -> 4
  Let{}      -> 4
  Apply (Bin b) [_,_] -> let (p, _, _) = binPrec b in p
  Apply{}    -> 14
  Function{} -> 14
  Switch{}   -> 14
  TyApply{}  -> 14
  Lit{}      -> 15
  Name{}     -> 15

instance PP Pattern where
  pp p = case p of
    ConP n ps -> emparens (pp n) (map pp ps)
    NameP n -> pp n
    Wild -> "_"
    AssignP n p -> parens (pp n <+> "=" $\ pp p)
    LitP l -> pp l
    GuardP p e -> error "todo: pretty print guard syntax"

instance PP Case where
  pp (Case ps e) = ("case" $\ csv (map pp ps) <> ":") $\ pp e

instance PP Lit where
  pp l = case l of
    Integer i -> integer i
    String s -> text (show s)
    Unit -> "()"

instance PP Bin where
  pp b = case b of
    Mul -> "*"
    Div -> "/"
    Mod -> "%"
    Add -> "+"
    Sub -> "-"
    Eq  -> "=="
    Ne  -> "!="
    Lt  -> "<"
    Le  -> "<="
    Gt  -> ">"
    Ge  -> ">="

binPrec :: Bin -> (Int, Int, Int)
binPrec b = case b of
  Mul -> (8, 8, 9)
  Div -> (8, 8, 9)
  Mod -> (8, 8, 9)
  Add -> (7, 7, 8)
  Sub -> (7, 7, 8)
  Eq  -> (5, 6, 6)
  Ne  -> (5, 6, 6)
  Lt  -> (5, 6, 6)
  Le  -> (5, 6, 6)
  Gt  -> (5, 6, 6)
  Ge  -> (5, 6, 6)

lprec :: Bin -> Int
lprec b = let (_, p, _) = binPrec b in p

rprec :: Bin -> Int
rprec b = let (_, _, p) = binPrec b in p

instance PP Type where pp = ppr 0
instance PPrec Type where
  ppr i t = case t of
    NameTy n -> pp n
    TyCon n ts -> pp n <> crocodile ts
    Arrow ts t es ->
      parIf (i > 0) $
        parIf singleton (csv (map (ppr prec) ts)) <+> "->" $\
        fsep (intersperse "!" (map (ppr 1) (t:es)))
      where
      singleton = length ts == 1
      prec = if singleton then 1 else 0

parIf :: Bool -> Doc -> Doc
parIf True  = parens
parIf False = id

