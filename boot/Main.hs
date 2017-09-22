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

continue :: [AST.Decl] -> IO ()
continue decls =
  do putStrLn (pretty (AST.Decls decls))
     v <- runI (top decls)
     putStrLn (ppShow v)
     exitSuccess

showTree :: Program -> IO ()
showTree tree =
  do putStrLn $ "\n[Abstract Syntax]\n\n" ++ show tree
     putStrLn $ "\n[Linearized tree]\n\n" ++ printTree tree

main :: IO ()
main =
  do args <- getArgs
     case args of
       []     -> run =<< getContents
       [file] -> run =<< readFile file
