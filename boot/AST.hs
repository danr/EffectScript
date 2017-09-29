{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses #-}
{-# LANGUAGE PatternGuards #-}
module AST where

import Data.Function
import Data.Generics.Geniplate
import Data.Set (Set)
import qualified Data.Set as S
import Data.Map (Map)
import qualified Data.Map as M

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

addHandler :: Name
addHandler = wired "addHandler"

removeHandler :: Name
removeHandler = wired "removeHandler"

getHandler :: Name
getHandler = wired "getHandler"

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
  | Let Pattern Expr
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
  = Function {
      fn_name   :: Maybe Name,
      fn_tvs    :: [Name],
      fn_params :: [Pattern `WithOptional` Type],
      fn_res_ty :: (Maybe Type),
      fn_body   :: Expr
    }
  | Lambda [Pattern] Expr
  | Lit Lit
  | Bin Bin
  | Name Name
  | DataCon Name
  | Op Name
  | Quote Name
  | Switch [Expr] [Case]
  | Apply Expr [Expr]
  | TyApply Expr [Type]
  | Seq Decl Expr
  | Expr `Sig` Type
  | Expr `Labels` [Name]
  deriving (Eq, Ord, Show)

decls :: [Decl] -> Expr -> Expr
decls ds e = foldr Seq e ds

declsOf :: Expr -> [Decl]
declsOf (Seq d ds) = d:declsOf ds
declsOf _          = []

spanDecls :: (Decl -> Bool) -> Expr -> ([Decl], Expr)
spanDecls p (Seq d rhs) | p d = let (ds, e) = spanDecls p rhs in (d:ds, e)
spanDecls _ e = ([], e)

data Pattern
  = ConP Name [Pattern]
  | NameP Name
  | Wild
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
  | Arrow [Type {- these could be named like in TS -}] Type [Type]
  deriving (Eq, Ord, Show)

unitTyConName :: Name
unitTyConName = wired "Unit"

topHandler :: Name
topHandler = wired "h_top"

instanceUniverseBi [t| (Expr, Decl) |]
instanceUniverseBi [t| ([Expr], Pattern) |]
instanceTransformBi [t| (Expr, Expr) |]
instanceTransformBi [t| (Pattern, Expr) |]

intuitTypes :: Expr -> Expr
intuitTypes e0 =
    transformBi
      (\ e -> case e of
        Name n | n `S.member` datacons -> DataCon n
               | n `S.member` ops -> Op n
        _ -> e)
  $ transformBi
      (\ p -> case p of
        NameP n | n `S.member` datacons -> ConP n []
        _ -> p)
  $ e0
  where
  datacons = S.fromList $ map con_name $ concat [ cs | Algebraic _ cs <- universeBi e0 ]
  ops      = S.fromList $ map con_name $ concat [ cs | Effect _ cs <- universeBi e0 ]

betaReduce :: Expr -> Expr
betaReduce =
  transformBi $
    \ e0 ->
      case e0 of
        Lambda ps rhs `Apply` es
          | all simple ps,
            S.null (bound_in_es `S.intersection` names_in_ps) ->
              substList (M.fromList [ (n,e) | (NameP n,e) <- ps `zip` es ]) rhs
          where
          bound_in_es = S.fromList [ n | NameP n <- universeBi es ]
          names_in_ps = S.fromList [ n | NameP n <- ps ]
          simple p = case p of
            Wild -> True
            NameP{} -> True
            _ -> False
        _ -> e0

  where
substList :: Map Name Expr -> Expr -> Expr
substList su =
  transformBi $
    \ e0 ->
      case e0 of
        Name n | Just e <- M.lookup n su -> e
        _ -> e0

