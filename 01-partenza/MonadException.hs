{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant lambda" #-}
{-# LANGUAGE InstanceSigs #-}

module MonadException where

type Exception = String
data E a where
    Raise   :: Exception -> E a
    Return :: a -> E a
    deriving (Show)

instance Functor E where
  fmap :: (a -> b) -> E a -> E b
  fmap f (Raise er) = Raise er
  fmap f (Return a) = Return (f a)

instance Applicative E where
  pure :: a -> E a
  pure = return
  (<*>) :: E (a -> b) -> E a -> E b
  (<*>) (Raise er) _           = Raise er
  (<*>) _ (Raise er)           = Raise er
  (<*>) (Return f) (Return a) = Return (f a)

instance Monad E where
  (>>=) :: E a -> (a -> E b) -> E b
  (>>=) (Raise er) f  = Raise er
  (>>=) (Return a) f = f a
  return :: a -> E a
  return = Return




