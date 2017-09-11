module AST where

import Data.Function

data Name = MkName { name_repr :: String, pos :: (Int, Int) }

wired :: String -> MkName
wired s = MkNamed s (0, 0)

instance Eq Name where (==) = (==) `on` name_repr

instance Ord Name where compare = compare `on` name_repr

instance Show Name where
  show = show . name_repr

data TypeHead
  = TypeHead Name [Name]
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
          con_args :: [Type `WithOptional` Name] }
  deriving (Eq, Ord, Show)

data a `WithOptional` b = a `With` b | Without a
  deriving (Eq, Ord, Show)

without :: a `WithOptional` b -> a
without (a `With` b) = a
without (Without a)  = a

data Expr
  = Function Name [Name] (Name `WithOptional` Type)
  | Lambda [Pattern] Expr
  | Let Pattern Expr
  | Unit
  | Lit Lit
  | Bin Bin
  | Name Name
  | Switch [Expr] [Case]
  | Apply Expr [Expr]
  | TyApply Expr [Type]
  | Decls [Decl]
  | Sig Expr Type
  deriving (Eq, Ord, Show)

data Pattern
  | ConP Name [Pattern]
  | NameP Name
  | Wild
  | GuardP Pattern Expr
  deriving (Eq, Ord, Show)

data Case = Case [Pattern] Expr
  deriving (Eq, Ord, Show)

data Lit = Integer Integer | String String
  deriving (Eq, Ord, Show)

data Bin = Mul | Div | Mod | Add | Sub | Eq | Ne | Lt | Le | Gt | Ge
  deriving (Eq, Ord, Show)

data Type
  = NameTy Name
  | TyCon Name [Type]
  | Arrow [Type] Type [Type]
  deriving (Eq, Ord, Show)

