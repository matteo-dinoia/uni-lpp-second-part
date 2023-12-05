{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use =<<" #-}
{-# HLINT ignore "Redundant lambda" #-}

module MonadID where

-- ADT che corrisponde alla monade identità
-- quindi è anche un funtore e una struttura
-- applicativa
import Prelude hiding ( Functor ( ID ))

data ID a where
  Id   :: a -> ID a
  deriving (Show)

instance Functor ID where
  fmap :: (a -> b) -> ID a -> ID b
  fmap f (Id x) = Id (f x)

instance Applicative ID where
  pure :: a -> ID a
  pure = Id
  (<*>) :: ID (a -> b) -> ID a -> ID b
  (<*>) (Id f) (Id x) = Id (f x)

instance Monad ID where
  (>>=) :: ID a -> (a -> ID b) -> ID b
  (>>=) x f = let Id v = x in f v

  return :: a -> ID a
  return = pure

join::ID (ID a) -> ID a
join (Id (Id x)) = Id x