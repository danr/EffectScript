module Main where

import System.Environment (getArgs)
import System.Exit (exitFailure, exitSuccess)

import Text.Show.Pretty

import Control.Monad

import LexEffectScript
import ParEffectScript
import PrintEffectScript
import AbsEffectScript

import AST
import Pretty
--import Interpret
import TrAST
import Fresh
import ANF
import SmallStep
--import EffectConvert

import ErrM
import Control.Monad.Writer

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
      do --putStrLn "\nParse Successful!"
         --showTree tree
         --putStrLn (ppShow tree)
         let ast = tr tree
         putStrLn "\n=== Initial tree ===\n"
         putStrLn (pretty ast)
         putStrLn (ppShow ast)
         putStrLn "\n=== Typed tree ===\n"
         let ast2 = intuitTypes ast
         putStrLn (pretty ast2)
         continue ast2

continue :: AST.Expr -> IO ()
continue e =
  do
--     putStrLn (pretty e)
     let puts = tell . (:[])
     mapM_ putStrLn $ runFresh $ execWriterT $
       do e_anf <- lift $ anf e
          puts "\n=== Administrative Normal Form ===\n"
          puts (pretty e_anf)
          let (top, m) = valueDeclsOf e_anf
          let go e0 = do me <- lift $ step top e0
                         case me of
                           Nothing -> return ()
                           Just e' ->
                             do puts "\n=== Step ===\n"
                                puts (pretty e')
                                go e'
          go m

          {-
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
     putStrLn "\n=== Effect Converted ===\n"
     putStrLn (pretty e_ec)
     putStrLn "\n=== Beta Reduced ===\n"
     let e_br = betaReduce e_ec
     putStrLn (pretty e_br)
     v <- runI (iExpr e_br)
     putStrLn (pretty v)
     exitSuccess
     -}

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
