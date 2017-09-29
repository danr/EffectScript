{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
module Interpret where

import Control.Monad
import Control.Monad.Reader
import Data.Maybe
import Data.List
import Text.Show.Functions
import Text.Show.Pretty (ppShow)

import AST
import Pretty
import Text.PrettyPrint

import Data.Map (Map)
import qualified Data.Map as M


newtype I a = I { unI :: ReaderT Env IO a }
  deriving (MonadReader Env, Monad, Functor, Applicative)

io :: IO a -> I a
io = I . liftIO

data Env = Env
  { scope :: Bindings
  }
  deriving Show

instance PP Env where
  pp = ppBindings . scope

ppBindings :: Bindings -> Doc
ppBindings bs = fsep $ punctuate comma
  [ pp (x `With` v) | (x,v) <- M.toList bs, interesting x]
  where interesting x = x `M.notMember` wiredBindings

noFun :: Bindings -> Bindings
noFun = M.filter (not . isFun)
  where
  isFun FunV{} = True
  isFun _ = False

wiredBindings :: Bindings
wiredBindings =
  M.fromList
    [ (trueName, boolV True)
    , (falseName, boolV False)
    , (wired "puts", putsBuiltin)
    , (wired "show", showBuiltin)
    , (addHandler, BuiltinV addH)
    , (removeHandler, BuiltinV removeH)
    , (getHandler, BuiltinV getH)
    , (topHandler, Handlers [])
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
  }

runI :: I Value -> IO Value
runI (I m) = runReaderT m initEnv

{-
-- need to do this using iExpr to handle top-level lets
top :: [Decl] -> I Value
top ds = iExpr (Decls (ds ++ [Expr (Apply (Name (wired "main")) [])]))
-}

runtimeError :: String -> I Value
runtimeError = throwError . ("Runtime error: " ++)

partialEffect :: String -> I Value
partialEffect s = runtimeError ("partial: " ++ s)
  -- iExpr (Apply (Name partialName) [Lit (String s)])

typeError :: String -> I Value
typeError = throwError . ("Type error: " ++)

todo :: String -> I Value
todo = throwError . ("TODO: " ++)

throwError :: String -> I Value
throwError = error -- s = callCC $ \ _ -> return (ErrV s)

data Value
  = ConV Name [Value]
  | QuoteV Name -- a quoted name: label of an effect operation
  | FunV [Pattern] Closure Expr -- its rec group is part of the closure
  | BuiltinV ([Value] -> I Value) -- could be relaxed to return IO Value
  | LitV Lit
  | ErrV String
  | Handlers [([Name], Value)]
  deriving Show

instance PP Value where
  pp (ConV k vs) = "ConV" $\ emparens (pp k) (map pp vs)
  pp (QuoteV k) = "QuoteV" $\ pp k
  pp (FunV ps (Closure closure) e) =
    "FunV" $\
    pp (Function Nothing [] (map Without ps) Nothing e) $\
      brackets (ppBindings (noFun closure))
  pp (BuiltinV _) = "BuiltinV"
  pp (LitV l) = "LitV" $\ pp l
  pp (ErrV s) = emparens "ErrV" [text s]
  pp (Handlers xs) = "Handlers" $\ brackets
    (fsep (punctuate comma [ fsep (map pp x) <> ":" $\ pp y | (x,y) <- xs]))

-- Bindings, Scope, Closure etc
type Bindings = Map Name Value

data Closure = Closure Bindings

instance Show Closure where
  show Closure{} = "HiddenClosure"

putsBuiltin :: Value
putsBuiltin = BuiltinV $ \ xs -> case xs of
  [LitV (String s)] -> io (putStrLn s) >> return (LitV Unit)
  _ -> typeError "puts"

showBuiltin :: Value
showBuiltin = BuiltinV $ \ xs -> case xs of
  [LitV x] -> return (LitV (String (showLit x)))
  _ -> typeError $ "show called with " ++ show (length xs) ++ " args"

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
-- match (AssignP n p) v = M.insert n v <$> match p v
match (LitP lp) (LitV l)
  | lp == l = Just M.empty
  | otherwise = Nothing
match GuardP{} _ = error "todo: Pattern guards not implemneted"
match _ _ = Nothing

matches :: [Pattern] -> [Value] -> Maybe Bindings
matches (p:ps) (v:vs) = M.union <$> match p v <*> matches ps vs
matches []     []     = Just M.empty
matches _      _      = Nothing


trace :: (PP a, PP b) => a -> I b -> I b
trace e m =
  do env <- ask
     -- io $ putStrLn (render ("evaluating:" $\ pp e $$ "in context:" $\ pp env)) --  -| EnvSt env st))
     r <- m
     -- io $ putStrLn (pretty (r <-- e))
     return r

iExpr :: Expr -> I Value
iExpr e0 =
  let interesting = case e0 of
                      Lit{} -> False
                      Bin{} -> False
                      Name{} -> False
                      DataCon{} -> False
                      Quote{} -> False
                      Function{} -> False
                      Lambda{} -> False
                      _ -> True
  in (if interesting then trace e0 else id) $
  -- add debug output of what the scope and the context is
  case e0 of
    f@Function{} -> funV f <$> asks scope

    Lambda ps e ->
      do closure <- asks scope
         return (FunV ps (Closure closure) e)

    Lit l ->
      do return (LitV l)

    Bin b ->
      do return (binOp b)

    DataCon dc ->
      do return (ConV dc [])

    Op{} ->
      do runtimeError $ "Interpretator doesn't know about effects, run effect convert"

    Name n ->
      do mv <- asks (M.lookup n . scope)
         case mv of
           Just v  -> return v
           Nothing -> runtimeError $ "unbound name: " ++ name_repr n

    Quote n ->
      do return (QuoteV n)

    Switch es cases ->
      do vs <- mapM iExpr es
         let go (Case ps rhs:cases') =
               do mbs <- return $ matches ps vs
                  case mbs of
                    Just bs -> extendScope bs (iExpr rhs)
                    Nothing -> go cases'
             go [] = partialEffect $ pretty e0 ++ "\n on value: " ++ pretty vs
         propagateErrors vs $ go cases

    Apply f es ->
      do vf <- iExpr f
         vs <- mapM iExpr es
         propagateErrors (vf:vs) $ case vf of
           ConV k [] -> return (ConV k vs)
           ConV k _  -> typeError "constructor already applied to arguments"

           FunV ps (Closure closure) rhs ->
             do mbs <- return $ matches ps vs
                case mbs of
                  Just bs -> inScope closure $ extendScope bs (iExpr rhs)
                  Nothing -> partialEffect $ "function argument: " ++ pretty ps ++ pretty vs

           ErrV x -> return (ErrV x)

           BuiltinV f -> f vs

           _ -> typeError "cannot apply anything to this value"

    TyApply e ts ->
      do iExpr e

    ds@(Seq d _) | declarative d ->
      do let (decls, e) = spanDecls declarative ds
         bs <- declsScope decls <$> asks scope
         extendScope bs $ iExpr e

    Seq (Let p e) ds ->
      do v <- iExpr e
         mb <- return $ match p v
         propagateError v $ case mb of
           Just bs -> extendScope bs $ iExpr ds
           Nothing -> partialEffect "let"

propagateErrors :: [Value] -> I Value -> I Value
propagateErrors (v:vs) m = propagateError v (propagateErrors vs m)
propagateErrors _ m = m

propagateError :: Value -> I Value -> I Value
propagateError v@ErrV{} _ = return v
propagateError _ m = m

orList :: [a] -> [a] -> [a]
orList [] ys = ys
orList xs _  = xs

declarative :: Decl -> Bool
declarative (Let _ Function{}) = True
declarative Let{} = False
declarative _     = True

funV :: Expr -> Bindings -> Value
funV Function{fn_name, fn_params, fn_body} closure = f
  where
  add_self = maybe id (`M.insert` f) fn_name
  f = FunV (map without fn_params)
           (Closure (add_self closure)) fn_body

-- These are all 'declarative'
declsScope :: [Decl] -> Bindings -> Bindings
declsScope ds closure = us
  where
  us = M.fromList [ (n, funV f (us `M.union` closure)) | Let (NameP n) f@Function{} <- ds ]

removeH :: [Value] -> I Value
removeH [Handlers xs, QuoteV op] =
  return $ Handlers [ x | x@(ops, _) <- xs, op `notElem` ops ]

addH :: [Value] -> I Value
addH (Handlers xs:k:qops) =
  return $ Handlers ((map (\ (QuoteV op) -> op) qops, k):xs)
addH xs =
  do io $ putStrLn "addH args:"
     mapM_ (io . putStrLn . pretty) xs
     error "addH wrongly applied"

getH :: [Value] -> I Value
getH [Handlers xs, QuoteV op] =
  case [ k | (ops, k) <- xs, op `elem` ops ] of
    k:_ -> return k
    [] -> error $ "Unregistered handler for " ++ pretty op

