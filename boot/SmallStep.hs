{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
module SmallStep where
import TrAST

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
import Data.Set (Set)
import qualified Data.Set as S

type WiredBindings = Map Name ([Value] -> Value)

wiredBindings :: WiredBindings
wiredBindings =
  M.fromList
    [ (wired "puts", putsBuiltin)
    , (wired "show", showBuiltin)
    {-
    , (trueName, boolV True)
    , (falseName, boolV False)
    , (addHandler, BuiltinV addH)
    , (removeHandler, BuiltinV removeH)
    , (getHandler, BuiltinV getH)
    , (topHandler, Handlers [])
    -}
    ]

boolV :: Bool -> Value
boolV True  = DataCon trueName []
boolV False = DataCon falseName []

{-
-- need to do this using iExpr to handle top-level lets
top :: [Decl] -> Value
top ds = iExpr (Decls (ds ++ [Expr (Apply (Name (wired "main")) [])]))
-}

runtimeError :: String -> Value
runtimeError = throwError . ("Runtime error: " ++)

partialEffect :: String -> Value
partialEffect s = runtimeError ("partial: " ++ s)
  -- iValue (Apply (Name partialName) [Lit (String s)])

typeError :: String -> Value
typeError = throwError . ("Type error: " ++)

todo :: String -> Value
todo = throwError . ("TODO: " ++)

throwError :: String -> Value
throwError s = Name (wired s) -- errorName -- [Lit (String s)]
    -- s = callCC $ \ _ -> return (ErrV s)

putsBuiltin :: [Value] -> Value
putsBuiltin = \ case
  [Lit (String s)] -> {- putStrLn s >> -} Lit Unit
  _ -> typeError "puts"

showBuiltin :: [Value] -> Value
showBuiltin = \ case
  [v] -> Lit (String (pretty v))
  xs  -> typeError $ "show called with " ++ show (length xs) ++ " args"

showLit :: Lit -> String
showLit (String s)  = show s
showLit (Integer i) = show i
showLit Unit        = "()"

binOp :: Bin -> [Value] -> Value
binOp bop =
  case bop of
    Mul -> intOp (*)
    Div -> intOp div
    Mod -> intOp mod
    Add -> intStringOp (+) (++)
    Sub -> intOp (-)
    Eq  -> \ case [x, y] -> maybe (typeError "bool value") boolV (valueEq x y)
                  _      -> typeError "=="
    Ne  -> \ case [x, y] -> maybe (typeError "bool value") (boolV . not) (valueEq x y)
                  _      -> typeError "!="
    Lt  -> intStringCmp (<)  (<)
    Le  -> intStringCmp (<=) (<=)
    Gt  -> intStringCmp (>)  (>)
    Ge  -> intStringCmp (>=) (>=)

intOp :: (Integer -> Integer -> Integer) -> [Value] -> Value
intOp op xs =
  case xs of
    [Lit (Integer x), Lit (Integer y)] -> Lit (Integer (op x y))
    _ -> throwError "intOp"

intStringOp :: (Integer -> Integer -> Integer) -> (String -> String -> String) -> [Value] -> Value
intStringOp i s xs =
  case xs of
    [Lit (Integer x), Lit (Integer y)] -> Lit (Integer (i x y))
    [Lit (String x), Lit (String y)] -> Lit (String (s x y))
    _ -> typeError "intStringOp"

intStringCmp :: (Integer -> Integer -> Bool) -> (String -> String -> Bool) -> [Value] -> Value
intStringCmp i s xs =
  case xs of
    [Lit (Integer x), Lit (Integer y)] -> boolV (i x y)
    [Lit (String x), Lit (String y)] -> boolV (s x y)
    _ -> typeError "intStringCmp"

valueEq :: Value -> Value -> Maybe Bool
valueEq (DataCon k1 vs1) (DataCon k2 vs2) = Just $ k1 == k2 && length vs1 == length vs2 && maybe False and (sequence (zipWith valueEq vs1 vs2))
valueEq (Lit x) (Lit y) = Just $ x == y
valueEq _ _ = Nothing

type Match = Map Name Value

match :: Pattern -> Value -> Maybe Match
match (ConP c ps) (DataCon c2 vs)
  | c == c2 = matches ps vs
  | otherwise = Nothing
match Wild _ = Just M.empty
match (NameP n) v = Just (M.singleton n v)
match (LitP lp) (Lit l)
  | lp == l = Just M.empty
  | otherwise = Nothing
match GuardP{} _ = error "todo: Pattern guards not implemneted"
match _ _ = Nothing

matches :: [Pattern] -> [Value] -> Maybe Match
matches (p:ps) (e:es) = M.union <$> match p e <*> matches ps es
matches []     _      = Just M.empty
matches _      _      = Nothing

just :: Expr -> Fresh (Maybe Expr)
just = return . Just

step :: Map Name Value -> Expr -> Fresh (Maybe Expr)
step top = \ case
  Function Nothing _ ps _ rhs `Apply` vs -> step top (Lambda (map without ps) rhs `Apply` vs)

  f0@(Function (Just f) _ ps _ rhs) `Apply` vs ->
    case matches (map without ps) vs of
      Just su -> let su' | f `M.notMember` top = M.insert f f0 su
                         | otherwise = su
                 in  Just <$> subst su' rhs
      Nothing -> return Nothing

  Lambda ps rhs `Apply` vs ->
    case matches ps vs of
      Just su -> Just <$> subst su rhs
      Nothing -> return Nothing

  DataCon n [] `Apply` vs -> just (Done (DataCon n vs))

  Bin b `Apply` vs -> just (Done (binOp b vs))

  Name n `Apply` vs | Just v <- M.lookup n top -> just (v `Apply` vs)

  Name n `Apply` vs | Just f <- M.lookup n wiredBindings -> just (Done (f vs))

  Let x (Done v1) `Seq` e2 -> case match x v1 of
                                Just b  -> Just <$> subst b e2
                                Nothing -> return Nothing
  Let x e1 `Seq` e2 -> fmap (\ e1' -> Let x e1' `Seq` e2) <$> step top e1

  _ `Seq` e2 -> step top e2

  Switch Nothing vs cs -> matchCases vs cs

  Switch (Just (Done v)) vs cs -> matchCases (DataCon doneName [v]:vs) cs

  Switch (Just e) vs cs
    | (Op l lvs, ctx) <- inCtx e
    , l `S.notMember` ctxLabels ctx
    -> do y <- fresh "y"
          vs' <- sequence [ fresh "v" | _ <- vs ]
          k <- refreshValue (Lambda (map NameP (y:vs'))
                             (Switch (Just (unCtx ctx (Name y))) (map Name vs') cs))
          step top (Switch Nothing (DataCon l lvs:vs ++ [k]) cs)

    | otherwise -> fmap (\ e' -> Switch (Just e') vs cs) <$> step top e

  e `Sig` _ -> step top e
  -- e `Labels` _ -> step e
  -- e `TyApply` _ -> step e

  _ -> return Nothing

data Ctx = Here | LetCtx Pattern Ctx Expr | Handle Ctx [Value] [Case]
  deriving (Eq, Ord, Show)

matchCases :: [Value] -> [Case] -> Fresh (Maybe Expr)
matchCases vs [] = return Nothing
matchCases vs (Case ps rhs:cases) =
  case matches ps vs of
    Nothing -> matchCases vs cases
    Just su -> Just <$> subst su rhs

inCtx :: Expr -> (Expr, Ctx)
inCtx (Let p e1 `Seq` e2) = case inCtx e1 of (e1', c) -> (e1', LetCtx p c e2)
inCtx (Switch (Just e) es cs) = case inCtx e of (e', c) -> (e', Handle c es cs)
inCtx e = (e, Here)

unCtx :: Ctx -> Value -> Expr
unCtx Here v = Done v
unCtx (LetCtx n c e2) e1 = Let n (unCtx c e1) `Seq` e2
unCtx (Handle c es cs) e = Switch (Just (unCtx c e)) es cs

ctxLabels :: Ctx -> Set Name
ctxLabels (LetCtx _ c _) = ctxLabels c
ctxLabels (Handle c _ cs) = ctxLabels c `S.union` dom cs
ctxLabels Here = S.empty

dom :: [Case] -> Set Name
dom cs = S.fromList [ p | Case (ConP p _:_) _ <- cs, p /= doneName ]

