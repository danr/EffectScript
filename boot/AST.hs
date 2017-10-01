{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ExplicitForAll #-}
module AST (module AST, module Fresh) where

import Data.Generics.Geniplate
import Data.Set (Set)
import qualified Data.Set as S
import Data.Map (Map)
import qualified Data.Map as M
import Data.Monoid
import Control.Monad.Writer
import Fresh

errorName :: Name
errorName = wired "error"

localName :: Name
localName = wired "local"

pairName :: Name
pairName = wired "pair"

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

data Value
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
  | DataCon Name [Value]
  | Unrestricted { restrict :: Expr }
  -- | Value `Labels` [Name]
  deriving (Eq, Ord, Show)

data Expr
  = Switch (Maybe Expr) [Value] [Case]
  | Apply Value [Value]
  | TyApply Value [Type]
  | Seq Decl Expr
  | Expr `Sig` Type
  | Op Name [Value]
  | Done Value
  deriving (Eq, Ord, Show)

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


instanceUniverseBi [t| (Expr, Decl) |]
instanceUniverseBi [t| ([Expr], Pattern) |]
instanceUniverseBi [t| ([Pattern], Pattern) |]
instanceTransformBi [t| (Expr, Expr) |]
instanceTransformBi [t| (Pattern, Expr) |]
instanceTransformBi [t| (Value, Expr) |]

transformExprM :: Monad m => (Expr -> m Expr) -> Expr -> m Expr
transformExprM = $(genTransformBiM' [t| forall m . (Expr -> m Expr) -> Expr -> m Expr |])

transformExprValueM :: Monad m => (Value -> m Value) -> Expr -> m Expr
transformExprValueM = $(genTransformBiM' [t| forall m . (Value -> m Value) -> Expr -> m Expr |])

transformPatternsM :: Monad m => (Pattern -> m Pattern) -> [Pattern] -> m [Pattern]
transformPatternsM = $(genTransformBiM' [t| forall m . (Pattern -> m Pattern) -> [Pattern] -> m [Pattern] |])


refreshValueShallow :: Value -> Fresh Value
refreshValueShallow =
  \ case
    Lambda ps rhs ->
      do (ps',rhs') <- refreshPatterns ps rhs
         return (Lambda ps' rhs')
    Function n tvs ps rty rhs0 ->
      do (np,rhs1) <- refreshPatterns [maybe Wild NameP n] rhs0
         let n' = case np of
                    [NameP x] -> Just x
                    [Wild] -> Nothing
                    _ -> error "refreshValueShallow impossible"
         (ps',rhs2) <- refreshPatterns (map without ps) rhs1
         return (Function n' tvs (map Without ps') rty rhs2)
    v -> return v

refresh :: Expr -> Fresh Expr
refresh =
  transformExprM $
    \ case
      Done v -> Done <$> refreshValueShallow v
      Apply v vs -> Apply <$> refreshValueShallow v <*> mapM refreshValueShallow vs
      TyApply v t -> (`TyApply` t) <$> refreshValueShallow v
      Let p e1 `Seq` e2 ->
        do ([p'], e2') <- refreshPatterns [p] e2
           Let p' e1 `mkSeq` e2'
      Switch me es cs ->
        do cs' <- sequence [ do (ps', rhs') <- refreshPatterns ps rhs
                                return (Case ps' rhs')
                           | Case ps rhs <- cs ]
           return (Switch me es cs')
      e0 -> return e0

refreshPatterns :: [Pattern] -> Expr -> Fresh ([Pattern], Expr)
refreshPatterns ps e =
  do (ps', Endo mk_su) <-
       runWriterT $ (`transformPatternsM` ps) $
         \ case
            NameP x -> do x' <- refreshName x
                          tell (Endo (M.insert x (Name x')))
                          return (NameP x')
            p -> return p
     e' <- subst (mk_su M.empty) e
     return (ps', e')

refreshValue :: Value -> Fresh Value
refreshValue v =
  do Done v' <- refresh (Done v)
     return v'

apply :: Value -> [Value] -> Expr
apply (DataCon c []) vs = Done (DataCon c vs)
apply v vs = Apply v vs

uApply :: Expr -> [Expr] -> Expr
uApply (Op c []) es = Op c (map unrestricted es)
uApply e es = unrestricted e `apply` map unrestricted es

unrestricted :: Expr -> Value
unrestricted (Done v) = v
unrestricted e = Unrestricted e

decls :: [Decl] -> Expr -> Expr
decls ds e = foldr Seq e ds

sequ :: Expr -> Expr -> Expr
sequ (Seq d ds) e2 = Seq d (sequ ds e2)
sequ e          e2 = Let Wild e `Seq` e2

declsOf :: Expr -> ([Decl], Expr)
declsOf (Seq d ds) = case declsOf ds of (ds', e) -> (d:ds', e)
declsOf e          = ([], e)

valueDeclsOf :: Expr -> (Map Name Value, Expr)
valueDeclsOf (Seq Algebraic{} ds) = valueDeclsOf ds
valueDeclsOf (Seq Alias{} ds) = valueDeclsOf ds
valueDeclsOf (Seq Effect{} ds) = valueDeclsOf ds
valueDeclsOf (Seq (Let (NameP x) (Done v)) ds) = case valueDeclsOf ds of
                                                   (m, e) -> (M.insert x v m, e)
valueDeclsOf e          = (M.empty, e)

{-
spanDecls :: (Decl -> Bool) -> Expr -> ([Decl], Expr)
spanDecls p (Seq d rhs) | p d = let (ds, e) = spanDecls p rhs in (d:ds, e)
spanDecls _ e = ([], e)
-}

unitTyConName :: Name
unitTyConName = wired "Unit"

topHandler :: Name
topHandler = wired "h_top"

intuitTypes :: Expr -> Expr
intuitTypes e0 =
    transformBi
      (\ e -> case e of
        Unrestricted (Op n []) `Apply` vs -> Op n vs
        _ -> e)
  $ transformBi
      (\ e -> case e of
        Name n | n `S.member` datacons -> DataCon n []
               | n `S.member` ops -> Unrestricted (Op n [])
        _ -> e)
  $ transformBi
      (\ p -> case p of
        NameP n | n `S.member` datacons -> ConP n []
        NameP n | n `S.member` ops -> ConP n []
        _ -> p)
  $ e0
  where
  datacons = S.fromList $ map con_name $ concat [ cs | Algebraic _ cs <- universeBi e0 ]
  ops      = S.fromList $ map con_name $ concat [ cs | Effect _ cs <- universeBi e0 ]

betaReduce :: Expr -> Fresh Expr
betaReduce =
  transformExprM $
    \ e0 ->
      case e0 of
        Lambda ps rhs `Apply` es
          | length ps == length es, all simple ps ->
              betaReduce =<<
              subst (M.fromList [ (n,e) | (NameP n,e) <- ps `zip` es ]) rhs
          where
          simple p = case p of
            Wild -> True
            NameP{} -> True
            _ -> False
        _ -> return e0

subst :: Map Name Value -> Expr -> Fresh Expr
subst su =
  transformExprValueM $
    \ case
      Name n | Just v <- M.lookup n su -> refreshValue v
      v0 -> return v0


{-

      handle p0, e1, e2 {
        case op(x1, x2), k, s1, s2: P
        case done(r), _, s1, s2: Q
      }

      =>

      loop(p, v1, v2) {
        switch next(p), v1, v2 {
          case op(x1, x2, k'), s1, s2: let k = (x', t1, t2) => loop({k'(x')}, t1, t2) in P
          case done(r, _), s1, s2: Q

        }
      }
      in loop(p0, e1, e2)

-}

{-
unswitch :: Expr -> Fresh Expr
unswitch =
  transformExprM $
    \ case
      Switch (Just p0) es cases ->
        do loop <- fresh "loop"
           cases' <-
             sequence
               [ do k <- case p_k of
                            NameP k -> return k
                            Wild    -> fresh "kwild"
                    k' <- fresh "k"
                    x' <- fresh "x"
                    ts <- sequence [ fresh "t" | _ <- es ]
                    return $
                      Case
                        (ConP op (xs ++ [NameP k']):ps)
                        (inlineLet
                          p_k (lam (x':ts)
                                   (loop $$ (lam []
                                               (k' $$ [Name x']):map Name ts)))
                          rhs)
               | Case (ConP op xs:p_k:ps) rhs <- cases
               ]
           let labels = [ op | Case (ConP op _:_) _ <- cases ]
           vs <- sequence [ fresh "v" | _ <- es ]
           p <- fresh "p"
           return $
             inlineLet
               (NameP loop)
               (Function (Just loop) [] (map (Without . NameP) (p:vs)) Nothing
                  (Switch Nothing
                     ((Name nextName `Labels` labels) `Apply` [Name p]:map Name vs)
                     cases'))
               (loop $$ (p0:es))
      e -> return e
    -}

f $$ xs = Apply (Name f) xs
lam ns rhs = Lambda (map NameP ns) rhs

-- inlineLet p e1 e2 = Lambda [p] e2 `Apply` [e1]
inlineLet p e1 e2 = Let p e1 `Seq` e2

mkSeq :: Decl -> Expr -> Fresh Expr
d `mkSeq` e = return (d `Seq` e)
