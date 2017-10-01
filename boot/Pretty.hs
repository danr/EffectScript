{-# LANGUAGE OverloadedStrings, TypeOperators #-}
module Pretty where

import AST hiding (($$))
import Text.PrettyPrint
import Data.List (intersperse)
import Text.Show.Pretty (ppDoc)
import Data.Map (Map)
import qualified Data.Map as M

instance PP a => PP [a] where
  pp = csv . map pp

data Showable a = Show a

instance Show a => PP (Showable a) where
  pp (Show x) = ppDoc x

{-
data Infix a b = Infix String a b

(<--) = Infix "<-"
(-|) = Infix "-|"
-

instance (PP a, PP b) => PP (Infix a b) where
  pp (Infix s a b) = pp a <+> text s $\ pp b
  -}

($\) :: Doc -> Doc -> Doc
d $\ d2 = sep [d, nest 2 d2]

infixr 2 $\

embrace :: Doc -> Doc -> Doc
embrace h b = ((h <+> "{") $$ nest 2 b) $$ "}"

emparens :: Doc -> [Doc] -> Doc
emparens h bs = cat [h <> "(", nest 2 (csv bs <> ")")]

csv :: [Doc] -> Doc
csv = sep . punctuate ","

crocodile :: PP a => [a] -> Doc
crocodile [] = empty
crocodile xs = "<" <> csv (map pp xs) <> ">"

class PP a where
  pp :: a -> Doc

class PPrec a where
  ppr :: Int -> a -> Doc

instance PP Name where
  pp (MkName n _) = text (if take 1 n == "#" then drop 1 n else n)
  pp (Refreshed n i) = pp n <> "_" <> int i

pretty :: PP a => a -> String
pretty = render . pp

instance PP TypeHead where
  pp th = case th of
    TypeHead t [] -> pp t
    TypeHead t ts -> pp t <> crocodile ts

instance PP Decl where
  pp d = case d of
    Algebraic th cs -> embrace ("type" <+> pp th) (vcat (map pp cs))
    Effect th cs -> embrace ("effect" <+> pp th) (vcat (map pp cs))
    Alias th t -> "alias" <+> pp th <+> "=" $\ pp t
    Let Wild e -> pp e
    Let p e -> pp p <+> "=" $\ pp e

instance PP Con where
  pp (Con x ts as mrt) =
    pp x <> crocodile ts
         <> parens (csv (map ppConParam as))
         <> mrt ? (":" $\)

(?) :: PP a => Maybe a -> (Doc -> Doc) -> Doc
Just a  ? f = f (pp a)
Nothing ? _ = empty

infix 9 ?

ppConParam :: Type `WithOptional` Name -> Doc
ppConParam (t `With` n) = pp (n `With` t)
ppConParam (Without a) = pp a

instance (PP a, PP b) => PP (a `WithOptional` b) where
  pp (a `With` b) = pp a <> ":" $\ pp b
  pp (Without a) = pp a

instance PP Expr where pp = ppr 0
instance PPrec Expr where
  ppr i e0 = parIf (prec e0 <= i) $ case e0 of
    Apply (Bin b) [v1,v2] -> ppr (lprec b) v1 <+> pp b <+> ppr (rprec b) v2 -- todo: precedences
    Switch Nothing  vs cs -> ("switch" $\ csv (map pp vs)) `embrace` (vcat (map pp cs))
    Switch (Just e) vs cs -> ("handle" $\ csv (pp e:map pp (vs))) `embrace` (vcat (map pp cs))
    Apply v vs -> emparens (ppr 5 v) (map pp vs)
    TyApply v ts -> ppr 15 v <> crocodile ts
    Op n vs -> emparens ("!" <> pp n) (map pp vs)
    Seq d e -> pp d $$ pp e
    Sig e t -> ppr 3 e <> ":" $\ pp t
    Done v -> "done" $\ ppr 14 v

instance PP Value where pp = ppr 0
instance PPrec Value where
  ppr i v0 = parIf (precV v0 <= i) $ case v0 of
    Function mf tvs ps mrt e ->
      ("function" $\ emparens (mf ? id <> crocodile tvs) (map pp ps))
      `embrace` pp e
    Lambda [] e -> embrace empty (pp e)
    Lambda ps ds@Seq{} -> embrace (parens (csv (map pp ps)) $\ "=>") (pp ds)
    Lambda ps e -> parIf (i > 12 || length ps > 1) (csv (map pp ps)) <+> "=>" $\ ppr 4 e
    Lit l -> pp l
    Bin b -> parens (pp b)
    Name n -> pp n
    DataCon n vs -> emparens ("mk" <> pp n) (map pp vs)
    Unrestricted e -> "^" <> ppr i e
    -- v `Labels` ls -> ppr 3 v <> "[" <> fsep (map pp ls) <> "]"

instance (PP a, PP b) => PP (Map a b) where
  pp bs =
     fsep $ punctuate comma
     [ pp (x `With` v) | (x,v) <- M.toList bs, interesting x]
       where interesting x = True -- x `M.notMember` wiredBindings

prec :: Expr -> Int
prec e = case e of
  Seq{}       -> 15
  Sig{}       -> 2
  Apply (Bin b) [_,_] -> let (p, _, _) = binPrec b in p
  Apply{}     -> 14
  Op{}        -> 14
  Switch{}    -> 14
  TyApply{}   -> 14
  Done v      -> precV v

precV :: Value -> Int
precV e = case e of
  Lambda{}       -> 4
  Function{}     -> 14
  Unrestricted e -> prec e
  _              -> 15


instance PP Pattern where
  pp p = case p of
    ConP n ps -> emparens ("mk" <> pp n) (map pp ps)
    NameP n -> pp n
    Wild -> "_"
    LitP l -> pp l
    GuardP p e -> error "todo: pretty print guard syntax"

instance PP Case where
  pp (Case ps e) = ("case" $\ csv (map pp ps) <> ":") $\ pp e

instance PP Lit where
  pp l = case l of
    Integer i -> integer i
    String s -> text s -- (show s)
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
        parIf (not singleton) (csv (map (ppr prec) ts)) <+> "->" $\
        fsep (intersperse "!" (map (ppr 1) (t:es)))
      where
      singleton = length ts == 1
      prec = if singleton then 1 else 0

parIf :: Bool -> Doc -> Doc
parIf True  = parens
parIf False = id

