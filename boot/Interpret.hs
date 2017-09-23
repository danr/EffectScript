{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
module Interpret where

import Control.Monad
-- import Control.Monad.Reader
-- import Control.Monad.State
-- import Control.Monad.Cont
import Data.Maybe
import Data.List
import Text.Show.Functions
import Text.Show.Pretty (ppShow)

import AST
import Pretty
import Text.PrettyPrint

import Data.Map (Map)
import qualified Data.Map as M

newtype I a = I { unI :: Env -> St -> (St -> a -> IO Value) -> IO Value }


withCont :: ((Value -> I Value) -> I Value) -> I Value
withCont = _

instance Functor I where fmap = liftM
instance Applicative I where pure = return; (<*>) = ap

local :: (Env -> Env) -> I a -> I a
local k (I m) = I $ \ env -> m (k env)

ask :: I Env
ask = I $ \ env st k -> k st env

asks :: (Env -> a) -> I a
asks f = fmap f ask

get :: I St
get = I $ \ env st k -> k st st

gets :: (St -> a) -> I a
gets f = fmap f get

modify :: (St -> St) -> I ()
modify p = I $ \ env st k -> k (p st) ()

put :: St -> I ()
put = modify . const

io :: IO a -> I a
io m = I $ \ env st k -> m >>= k st

callCC :: ((a -> I b) -> I a) -> I a
callCC cc = I $ \ env st k -> let I m = cc (\ a -> I $ \ env st _ -> k st a) in m env st k


--   deriving (MonadIO, MonadCont, MonadReader Env, MonadState St, Monad, Functor, Applicative)

type Handlers = [([Name], Value -> I Value)]

data Env = Env
  { scope :: Bindings
  }
  deriving Show

data St = St
  { handlers :: Handlers
  }
  deriving Show

ppBindings :: Bindings -> Doc
ppBindings bs = fsep $ punctuate comma
  [ pp (x `With` v) | (x,v) <- M.toList bs, interesting x]
  where interesting x = x `M.notMember` wiredBindings

noFun :: Bindings -> Bindings
noFun = M.filter (not . isFun)
  where
  isFun FunV{} = True
  isFun EffectV{} = True
  isFun _ = False

data EnvSt = EnvSt Env St

instance PP EnvSt where
  pp (EnvSt Env{..} St{..}) = braces $ sep $ punctuate comma
    [ "scope:" $\ braces (ppBindings (noFun scope))
    , "handlers:" $\ brackets (csv [brackets (csv (map pp ns)) | (ns, _k) <- handlers ])
    ]

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
  }

initSt :: St
initSt = St
  { handlers = [([partialName], \ _ -> typeError "unhandled partial")]
  }

runI :: I Value -> IO Value
runI (I m) = m initEnv initSt (\ st a -> return a)

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
throwError = error -- s = callCC $ \ _ -> return (ErrV s)

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

instance PP Value where
  pp (ConV k vs) = emparens (pp k) (map pp vs)
  pp (FunV ps (Closure closure) e) =
    pp (Function Nothing [] (map Without ps) Nothing e) $\
      brackets (ppBindings (noFun closure))
  pp (NextV ops) = emparens "NextV" (map pp ops)
  pp (EffectV op) = emparens "EffectV" [pp op]
  pp (ContV _) = "ContV"
  pp (BuiltinV _) = "BuiltinV"
  pp (LitV l) = pp l
  pp (ErrV s) = emparens "ErrV" [text s]

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
match (AssignP n p) v = M.insert n v <$> match p v
match (LitP lp) (LitV l)
  | lp == l = Just M.empty
  | otherwise = Nothing
match GuardP{} _ = error "todo: Pattern guards not implemneted"
match _ _ = Nothing

matches :: [Pattern] -> [Value] -> Maybe Bindings
matches (p:ps) (v:vs) = M.union <$> match p v <*> matches ps vs
matches []     []     = Just M.empty
matches _      _      = Nothing

partialEffect :: I Value
partialEffect = iExpr (Apply (Name partialName) [])

trace :: (PP a, PP b) => a -> I b -> I b
trace e m =
  do env <- ask
     st <- get
     io $ putStrLn (render ("evaluating:" $\ pp e $$ "in context:" $\ pp (EnvSt env st))) --  -| EnvSt env st))
     r <- m
     io $ putStrLn (pretty (r <-- e))
     return r

iExpr :: Expr -> I Value
iExpr = iExprWithInfo Nothing

data WithInfo = Expr `WithInfo` Maybe [Name]

instance PP WithInfo where
 pp (e `WithInfo` Nothing) = pp e
 pp (e `WithInfo` Just ns) = pp e $\ brackets (brackets (csv (map pp ns)))

iExprWithInfo :: Maybe [Name] -> Expr -> I Value
iExprWithInfo minfo e0 =
  let interesting = case e0 of
                      Lit{} -> False
                      Bin{} -> False
                      Name{} -> False
                      Function{} -> False
                      Lambda{} -> False
                      _ -> True
  in (if interesting then trace (e0 `WithInfo` minfo) else id) $
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
         propagateErrors vs $ go cases

    Apply f es ->
      do vf <- iExprWithInfo minfo f
         vs <- mapM iExpr es
         propagateErrors (vf:vs) $ case vf of
           ConV k [] -> return (ConV k vs)
           ConV k _  -> typeError "constructor already applied to arguments"

           ContV k -> do
             case vs of
               [v] -> k v >>= \ r -> io (putStrLn "ContV done") >> return r
               []  -> k (LitV Unit) >>= \ r -> io (putStrLn "ContV done") >> return r
               _   -> typeError "too many resume arguments"

           EffectV op ->
             do hs <- gets handlers
                case break ((op `elem`) . fst) hs of
                  (pre, (_ops, h):post) ->
                    callCC $ \ k ->
                      do io (putStrLn "CallCC from EffectV")
                         let k' v = modify (\ st -> st{ handlers = pre ++ post }) >> k v
                         h (ConV op (vs ++ [ContV k']))
                  _ -> typeError ("Unhandled handler: " ++ pretty op ++ " (from " ++ ppShow hs ++ ")")

           NextV ops ->
             let next m =
                   withCont $ \ n ->
                     do io (putStrLn "CallCC from NextV")
                        st0 <- get
                        modify $ \st -> st{
                          handlers = (ops, \ _ -> put st >> n):handlers st
                        }
                        ConV doneName . (:[]) <$> m
             in case vs of
               [FunV [] (Closure closure) rhs] -> inScope closure (next (iExpr rhs))
               [ContV k] -> next (k (LitV Unit))
               _ -> typeError $ "next must be applied to one single nullary function, is applied to " ++ ppShow vs


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
         propagateError v $ case mb of
           Just bs -> extendScope bs $ iExpr (Decls (ds `orList` [Expr e]))
           Nothing -> partialEffect

    Decls [Expr e] -> iExpr e

    Decls (Expr e:ds) ->
      do v <- iExpr e
         propagateError v $ iExpr (Decls ds)
        -- this expr is just done for side effects (cannot extend scope over ds)

    Decls [] -> return (LitV Unit)
        -- this can be the rhs of an empty block or lambda (e => {})
        -- not an empty record though :)


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

