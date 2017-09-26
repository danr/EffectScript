module Main where

import System.Environment (getArgs)
import System.Exit (exitFailure, exitSuccess)

import Text.Show.Pretty

import Control.Monad

import LexEffectScript
import ParEffectScript
import SkelEffectScript
import PrintEffectScript
import AbsEffectScript

import AST
import Pretty
import Interpret
import TrAST
import Fresh
import ANF
import EffectConvert

import ErrM

type ParseFun a = [Token] -> Err a

run :: String -> IO ()
run s =
  let ts = myLexer s in
  case pProgram ts of
    Bad s ->
      do putStrLn "\nParse Failed...\n"
         putStrLn "Tokens:"
         putStrLn $ show ts
         putStrLn s
         exitFailure
    Ok tree ->
      do putStrLn "\nParse Successful!"
         showTree tree
         putStrLn (ppShow tree)
         let ast = tr tree
         putStrLn (ppShow ast)
         continue ast

continue :: AST.Expr -> IO ()
continue e =
  do putStrLn (pretty e)
     let (e_anf, e_ec) =
           runFresh $
             do e_anf <- anf e
                x <- fresh "x"
                h' <- fresh "h"
                h'' <- fresh "h"
                f <- fresh "f"
                e_ec <- runEC $ ec e_anf
                    (AST.Name topHandler)
                    (AST.Lambda
                        [AST.NameP h', AST.NameP f]
                        (AST.Apply (AST.Name f) [AST.Name h',
                            AST.Lambda [AST.NameP h'', AST.NameP x] (AST.Name x)]))
                return (e_anf, e_ec)
     putStrLn "\n=== Administrative Normal Form ===\n"
     putStrLn (pretty e_anf)
     putStrLn "\n=== Effect Converted ===\n"
     putStrLn (pretty e_ec)
     v <- runI (iExpr e_ec)
     putStrLn (pretty v)
     exitSuccess

showTree :: Program -> IO ()
showTree tree =
  do putStrLn $ "\n[Abstract Syntax]\n\n" ++ show tree
     putStrLn $ "\n[Linearized tree]\n\n" ++ printTree tree

main :: IO ()
main =
  do args <- getArgs
     run =<< case args of
       []     -> getContents
       [file] -> readFile file
