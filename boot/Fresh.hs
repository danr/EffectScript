{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Fresh where

import Control.Monad.State
import Control.Monad
import Numeric
import Control.Monad.Writer

import Data.Function

wired :: String -> Name
wired s = MkName s (Pos 0 0)

data Pos = Pos Int Int
  deriving (Show)

instance Eq Pos where _ == _ = True
instance Ord Pos where _ `compare` _ = EQ

data Name
  = MkName { name_repr :: String, pos :: Pos }
  | Refreshed Name Int
  deriving (Eq, Ord, Show)

refreshed (Refreshed n _) i = refreshed n i
refreshed n i = Refreshed n i

newtype Fresh a = Fresh (State Int a)
  deriving (Monad, Applicative, Functor)

class Monad m => MonadFresh m where
  refreshName :: Name -> m Name

fresh :: MonadFresh m => String -> m Name
fresh = refreshName . wired

instance MonadFresh Fresh where
  refreshName n = Fresh $
    do i <- get
       modify succ
       return (refreshed n i)

instance (Monoid e, MonadFresh m) => MonadFresh (WriterT e m) where
  refreshName = lift . refreshName

runFresh :: Fresh a -> a
runFresh (Fresh m) = evalState m 0
