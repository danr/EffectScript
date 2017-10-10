module Main where

import System.Environment (getArgs)
import System.Exit (exitFailure, exitSuccess)

import Text.Show.Pretty

import Control.Monad

import LexEffectScript
import ParEffectScript
import PrintEffectScript
import AbsEffectScript
import Data.List

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

run :: [String] -> String -> IO ()
run flags s =
  let ts = myLexer s in
  case pProgram ts of
    Bad s ->
      do putStrLn "\nParse Failed...\n"
         putStrLn "Tokens:"
         putStrLn $ show ts
         putStrLn s
         exitFailure
    Ok tree ->
      do -- putStrLn "\nParse Successful!"
         -- putStrLn (ppShow tree)
         let ast = tr tree
         -- putStrLn "\n=== Initial tree ===\n"
         -- putStrLn (pretty ast)
         -- putStrLn (ppShow ast)
         --putStrLn "\n=== Typed tree ===\n"
         let ast2 = intuitTypes ast
         --putStrLn (pretty ast2)
         continue flags ast2

continue :: [String] -> AST.Expr -> IO ()
continue flags e =
  do
--     putStrLn (pretty e)
     let puts = tell . (:[])
     let (final_e, msgs) = runFresh $ runWriterT $
           do e_anf <- lift $ anf e
              puts "\n=== Administrative Normal Form ===\n"
              puts (pretty e_anf)
              let (top, m) = valueDeclsOf e_anf
              puts ("\n== Top-level declarations ==\n" ++ pretty top)
              let go e0 = do me <- lift $ step top e0
                             case me of
                               Nothing -> return e0
                               Just e' ->
                                 do puts "\n=== Step ===\n"
                                    puts (pretty e')
                                    go e'
              go m
     when ("-steps" `elem` flags) $ mapM_ putStrLn msgs
     case final_e of
       Done v -> putStrLn (pretty v)
       _ -> putStrLn (pretty final_e)

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
  do (flags, args) <- partition (("-" ==) . take 1) <$> getArgs
     run flags =<< case args of
       []     -> getContents
       [file] -> readFile file
