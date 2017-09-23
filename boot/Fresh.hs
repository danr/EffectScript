module Fresh where

import Control.Monad.State
import Control.Monad
import AST

newtype Fresh a = Fresh (State Int a)
  deriving (Monad, Applicative, Functor)

class MonadFresh m where
  fresh :: String -> m Name

instance MonadFresh Fresh where
  fresh h = M $
    do i <- get
       modify succ
       return (wired ("#" ++ h ++ show i))

