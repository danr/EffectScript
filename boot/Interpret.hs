{-# LANGUAGE ParallelListComp #-}
{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleContexts #-}
module Interpret where

import Control.Monad
import Control.Monad.Reader
import Control.Monad.Cont
import Data.Maybe
import Data.List
import Text.Show.Functions

import AST
import Pretty

import Data.Map (Map)
import qualified Data.Map as M

newtype I a = I (ReaderT Env (ContT Value IO) a)
  deriving (MonadIO, MonadCont, MonadReader Env, Monad, Functor, Applicative)

data Env = Env
  { scope :: Bindings
  , handlers :: [([Name], Value -> I Value)]
  }
  deriving Show

wiredBindings :: Bindings
wiredBindings =
  M.fromList
    [ (partialName, EffectV partialName)
    , (trueName, boolV True)
    , (falseName, boolV False)
    , (wired "puts", putsBuiltin)
    , (wired "show", showBuiltin)
    ]

extendScope :: Bindings -> I a -> I a
extendScope bs = local (\ env -> env{scope = bs `M.union` scope env})

initScope :: I a -> I a
initScope = local (\ env -> env{scope = scope initEnv})

inScope :: Bindings -> I a -> I a
inScope bs = initScope . extendScope bs

initEnv :: Env
initEnv = Env
  { scope = wiredBindings
  , handlers = [([partialName], \ _ -> typeError "unhandled partial")]
  }

runI :: I Value -> IO Value
runI (I m) = runContT (runReaderT m initEnv) return

-- need to do this using iExpr to handle top-level lets
top :: [Decl] -> I Value
top ds = iExpr (Decls (ds ++ [Expr (Apply (Name (wired "main")) [])]))

runtimeError :: String -> I Value
runtimeError = throwError . ("Runtime error: " ++)

typeError :: String -> I Value
typeError = throwError . ("Type error: " ++)

todo :: String -> I Value
todo = throwError . ("TODO: " ++)

throwError :: String -> I Value
throwError s = callCC $ \ _ -> return (ErrV s)

data Value
  = ConV Name [Value]
  | FunV [Pattern] Closure Expr -- its rec group is part of the closure
  | NextV [Name] -- ops this next handles
  | EffectV Name -- the op this effect yields when applied to its arguments
  | ContV (Value -> I Value) -- continuation from next
  | BuiltinV ([Value] -> I Value) -- could be relaxed to return IO Value
  | LitV Lit
  | ErrV String
  deriving Show

-- Bindings, Scope, Closure etc
type Bindings = Map Name Value

data Closure = Closure Bindings | Hidden

instance Show Closure where
  show Hidden = "<Hidden>"
  show (Closure m) = show (M.map hide m)
    where
    hide (FunV ps _ e) = FunV ps Hidden e
    hide v = v

putsBuiltin :: Value
putsBuiltin = BuiltinV $ \ xs -> case xs of
  [LitV (String s)] -> liftIO (putStrLn s) >> return (LitV Unit)
  _ -> typeError "puts"

showBuiltin :: Value
showBuiltin = BuiltinV $ \ xs -> case xs of
  [LitV x] -> return (LitV (String (showLit x)))
  _ -> typeError "show"

showLit :: Lit -> String
showLit (String s)  = show s
showLit (Integer i) = show i
showLit Unit        = "()"

binOp :: Bin -> Value
binOp bop =
  case bop of
    Mul -> intOp (*)
    Div -> intOp (*)
    Mod -> intOp mod
    Add -> intStringOp (+) (++)
    Sub -> intOp (-)
    Eq  -> BuiltinV $ \ xs -> case xs of [x, y] -> fromMaybe (typeError "bool value") (fmap (return . boolV) (valueEq x y))
                                         _      -> typeError "=="
    Ne  -> BuiltinV $ \ xs -> case xs of [x, y] -> fromMaybe (typeError "bool value") (fmap (return . boolV . not) (valueEq x y))
                                         _      -> typeError "!="
    Lt  -> intStringCmp (<)  (<)
    Le  -> intStringCmp (<=) (<=)
    Gt  -> intStringCmp (>)  (>)
    Ge  -> intStringCmp (>=) (>=)

intOp :: (Integer -> Integer -> Integer) -> Value
intOp op = BuiltinV $ \ xs ->
  case xs of
    [LitV (Integer x), LitV (Integer y)] -> return $ LitV (Integer (op x y))
    _ -> throwError "intOp"

intStringOp :: (Integer -> Integer -> Integer) -> (String -> String -> String) -> Value
intStringOp i s = BuiltinV $ \ xs ->
  case xs of
    [LitV (Integer x), LitV (Integer y)] -> return $ LitV (Integer (i x y))
    [LitV (String x), LitV (String y)] -> return $ LitV (String (s x y))
    _ -> typeError "intStringOp"

intStringCmp :: (Integer -> Integer -> Bool) -> (String -> String -> Bool) -> Value
intStringCmp i s = BuiltinV $ \ xs ->
  case xs of
    [LitV (Integer x), LitV (Integer y)] -> return $ boolV (i x y)
    [LitV (String x), LitV (String y)] -> return $ boolV (s x y)
    _ -> typeError "intStringCmp"

valueEq :: Value -> Value -> Maybe Bool
valueEq (ConV k1 vs1) (ConV k2 vs2) = Just $ k1 == k2 && length vs1 == length vs2 && maybe False and (sequence (zipWith valueEq vs1 vs2))
valueEq (LitV x) (LitV y) = Just $ x == y
valueEq _ _ = Nothing

boolV :: Bool -> Value
boolV True  = ConV trueName []
boolV False = ConV falseName []


match :: Pattern -> Value -> Maybe Bindings
match (ConP c ps) (ConV c2 vs)
  | c == c2 = matches ps vs
  | otherwise = Nothing
match Wild _ = Just M.empty
match (NameP n) v = Just (M.singleton n v)
match (AssignP n p) v = M.insert n v <$> match p v
match (LitP lp) (LitV l)
  | lp == l = Just M.empty
  | otherwise = Nothing
match GuardP{} _ = error "todo: Pattern guards not implemneted"

matches :: [Pattern] -> [Value] -> Maybe Bindings
matches (p:ps) (v:vs) = M.union <$> match p v <*> matches ps vs
matches []     []     = Just M.empty
matches _      _      = Nothing

-- Run until some of these are handled
withHandlersFor :: [Name] -> I Value -> I Value
withHandlersFor ops m =
  callCC $ \ k ->
    do local (\ env -> env{ handlers = (ops, k):handlers env }) m

handlerFor :: Name -> I (Value -> I Value)
handlerFor op =
  do (pre, (_, m):post) <- asks (span ((op `elem`) . fst) . handlers)
     return $ \ x -> local (\ env -> env{ handlers = pre ++ post }) (m x)

partialEffect :: I Value
partialEffect = iExpr (Apply (Name partialName) [])

trace :: (PP a, Show b) => a -> I b -> I b
trace e m =
  do liftIO $ putStrLn (pretty e)
     (liftIO . putStrLn . pretty . Show) =<< ask
     r <- m
     liftIO $ putStrLn (pretty (Show r :<- e))
     return r

iExpr :: Expr -> I Value
iExpr = iExprWithInfo Nothing

iExprWithInfo :: Maybe [Name] -> Expr -> I Value
iExprWithInfo minfo e0 =
  trace e0 $
  -- add debug output of what the scope and the context is
  case e0 of
    Function{} -> iExpr (Decls [Expr e0])

    Lambda ps e ->
      do closure <- asks scope
         return (FunV ps (Closure closure) e)

    Let{} ->
      todo "Let not at top level: won't scope over anything (nested let not supported by interpreter)"

    Lit l ->
      do return (LitV l)

    Bin b ->
      do return (binOp b)

    Name n | n == nextName ->
      case minfo of
        Just info -> return (NextV info)
        Nothing   -> runtimeError "next must be head of a scrutinee in the untyped interpreter"
    Name n ->
      do mv <- asks (M.lookup n . scope)
         case mv of
           Just v  -> return v
           Nothing -> runtimeError $ "unbound name: " ++ name_repr n

    Switch es cases ->
      do vs <- sequence [ iExprWithInfo (Just [c | ConP c _ <- pats]) e
                        | e <- es
                        | pats <- transpose [ps | Case ps _ <- cases] ]
         let go (Case ps rhs:cases') =
               do mbs <- return $ matches ps vs
                  case mbs of
                    Just bs -> extendScope bs (iExpr rhs)
                    Nothing -> go cases'
             go [] = partialEffect
         go cases

    Apply f es ->
      do vf <- iExpr f
         vs <- mapM iExpr es
         case vf of
           ConV k [] -> return (ConV k vs)
           ConV k _  -> typeError "constructor already applied to arguments"

           ContV k ->
             case vs of
                [v] -> k v
                []  -> k (LitV Unit)
                _   -> typeError "too many resume arguments"

           EffectV c ->
             do h <- handlerFor c
                callCC $ \ k -> h (ConV c (vs ++ [ContV k]))

           NextV ops ->
             case vs of
               [FunV [] (Closure closure) rhs] ->
                 do inScope closure $ do
                      withHandlersFor ops $ do
                        (ConV doneName . (:[])) <$> iExpr rhs
               _ -> typeError "next must be applied to one single nullary function"

           FunV ps (Closure closure) rhs ->
             do mbs <- return $ matches ps vs
                case mbs of
                  Just bs -> inScope closure $ extendScope bs (iExpr rhs)
                  Nothing -> partialEffect

           ErrV x -> return (ErrV x)

           BuiltinV f -> f vs

           _ -> typeError "cannot apply anything to this value"

    TyApply e ts ->
      do iExpr e

    Decls ds@(d:_) | declarative d ->
      do let (decls,expr_decls) = span declarative ds
         closure <- asks scope
         bs <- return $ declsScope decls closure
         extendScope bs $ iExpr (Decls (expr_decls `orList` [last ds]))

    Decls (Expr (Let p e):ds) ->
      do v <- iExpr e
         mb <- return $ match p v
         case mb of
           Just bs -> extendScope bs $ iExpr (Decls (ds `orList` [Expr e]))
           Nothing -> partialEffect

    Decls [Expr e] -> iExpr e

    Decls (Expr e:ds) -> iExpr e >> iExpr (Decls ds)
        -- this expr is just done for side effects (cannot extend scope over ds)

    Decls [] -> return (LitV Unit)
        -- this can be the rhs of an empty block or lambda (e => {})
        -- not an empty record though :)

orList :: [a] -> [a] -> [a]
orList [] ys = ys
orList xs _  = xs

declarative :: Decl -> Bool
declarative (Expr Function{}) = True
declarative (Expr _) = False
declarative _        = True

-- These are all 'declarative'
declsScope :: [Decl] -> Bindings -> Bindings
declsScope ds closure = us
  where
  us = M.fromList $
        [ (n, FunV (map without ps) (Closure (us `M.union` closure)) rhs)
        | Expr (Function (Just n) _tvs ps _ rhs) <- ds ] ++
        [ (n, ConV n []) | Algebraic _ cons <- ds, Con{con_name=n} <- cons ] ++
        [ (n, EffectV n) | Effect _ cons <- ds, Con{con_name=n} <- cons ]

