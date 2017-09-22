{-# LANGUAGE TypeOperators #-}
module AST where

import Data.Function

data Name = MkName { name_repr :: String, pos :: (Int, Int) }

wired :: String -> Name
wired s = MkName s (0, 0)

trueName :: Name
trueName = wired "true"

falseName :: Name
falseName = wired "false"

doneName :: Name
doneName = wired "done"

partialName :: Name
partialName = wired "partial"

nextName :: Name
nextName = wired "next"

wildName :: Name
wildName = wired "_"

instance Eq Name where (==) = (==) `on` name_repr

instance Ord Name where compare = compare `on` name_repr

instance Show Name where
  show = show . name_repr

data TypeHead
  = TypeHead { ty_con :: Name, ty_tvs :: [Name] }
  deriving (Eq, Ord, Show)

data Decl
  = Algebraic TypeHead [Con]
  | Effect TypeHead [Con]
  | Alias TypeHead Type
  | Expr Expr
  deriving (Eq, Ord, Show)

data Con
  = Con { con_name :: Name,
          con_quant :: [Name],
          con_args :: [Type `WithOptional` Name],
          con_res_ty :: Maybe Type }
  deriving (Eq, Ord, Show)

data a `WithOptional` b = a `With` b | Without a
  deriving (Eq, Ord, Show)

without :: a `WithOptional` b -> a
without (a `With` b) = a
without (Without a)  = a

data Expr
  = Function (Maybe Name) [Name] [Pattern `WithOptional` Type] (Maybe Type) Expr
  | Lambda [Pattern] Expr
  | Let Pattern Expr
  | Lit Lit
  | Bin Bin
  | Name Name
  | Switch [Expr] [Case]
  | Apply Expr [Expr]
  | TyApply Expr [Type]
  | Decls [Decl]
  | Sig Expr Type
  deriving (Eq, Ord, Show)

decls :: [Decl] -> Expr
decls [Expr e] = e
decls ds       = Decls ds

data Pattern
  = ConP Name [Pattern]
  | NameP Name
  | Wild
  | AssignP Name Pattern
  | LitP Lit
  | GuardP Pattern Expr
  deriving (Eq, Ord, Show)

data Case = Case [Pattern] Expr
  deriving (Eq, Ord, Show)

data Lit = Integer Integer | String String | Unit
  deriving (Eq, Ord, Show)

data Bin = Mul | Div | Mod | Add | Sub | Eq | Ne | Lt | Le | Gt | Ge
  deriving (Eq, Ord, Show)

data Type
  = NameTy Name
  | TyCon Name [Type]
  | Arrow [Type] Type [Type]
  deriving (Eq, Ord, Show)

unitTyConName :: Name
unitTyConName = wired "Unit"

