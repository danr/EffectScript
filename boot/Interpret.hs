module Interpret where

data B


iExpr :: Expr -> Value
iExpr e0 =
  case e0 of
    Function f tvs ps ->
      do todo "bind f to f as value (with current scope in closure)"
         todo "return f as value"

    Lambda ps e ->
      do todo "return ps -> e as value"

    Let p e ->
      do v <- iExpr e0
         mbs <- match p v
         case mbs of
           Just bs ->
             extendScope bs
           Nothing ->
             effect partialEffect

    Unit ->
      do return UnitV

    Lit l ->
      do return (litV l)

    Bin b ->
      do return (binV b)

    Name n ->
      do -- n is bound to either:
         -- variable to Value
         -- top-level constructor, function or effect constructor
         v <- lookup n
         -- on failure program is not well-scoped

         case v of


    Switch es cs -> -- type information for effect handlers
      do vs <- mapM iExpr es
         let go (Case ps rhs:cs') =
               do mbs <- matches ps vs
                  case mbs of
                    Just bs ->
                      extendScope bs
                      iEval e
                    Nothing -> go cs'
             go [] = effect partialEffect
         go cs

    Apply f es ->
      do vf <- iExpr f
         vs <- mapM iExpr es
         case vf of
           VCont m | [] <- vs -> m
           VCont m -> todo "effects with arguments"

           VEffect c ... ->
             h <- handlerFor c
             callCC $ \ k -> h (VCon c (vs ++ [VCont k])

           VNext -> -- need some type information
             case vs of
               [VFun [] closure rhs] ->
                 do inEnv closure $ do
                                   -- cs the effect constructors we understand
                      withHandlersFor cs $ do
                         iExpr rhs

           VFun ps closure rhs ->   -- types?
             mbs <- matches ps vs
             case mbs of
               Just bs ->
                 inEnv closure $
                   do extendScope bs
                      iEval e
               Nothing -> effect partialEffect
           _ -> error "program is not well typed"

    TyApply e ts ->
      do iExpr e -- e should evaluate to something polymorphic

    Decls ds ->
      case splitAt (length ds - 1) ds of
        (_, []) -> error "empty list of decls (rhs)"
