{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Fresh where

import Control.Monad.State
import Control.Monad
import AST
import Numeric

newtype Fresh a = Fresh (State Int a)
  deriving (Monad, Applicative, Functor)

class MonadFresh m where
  fresh :: String -> m Name

instance MonadFresh Fresh where
  fresh h = Fresh $
    do i <- get
       modify succ
       return (wired ("#" ++ h ++ show i))
--       return (wired ("#" ++ h ++ "" ++ showIntAtBase 36 ((['0'..'9'] ++ ['a'..'z']) !!) i ""))

runFresh :: Fresh a -> a
runFresh (Fresh m) = evalState m 0
