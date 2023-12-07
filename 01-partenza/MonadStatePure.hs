{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use tuple-section" #-}
{-# HLINT ignore "Redundant lambda" #-}
{-# HLINT ignore "Use camelCase" #-}
{-# LANGUAGE InstanceSigs #-}

module MonadStatePure where

type State = Integer
newtype S a = S (State -> (a, State))

instance Functor S where
  fmap :: (a -> b) -> S a -> S b
  fmap f (S x) = S (\s -> let (x', s') = x s
                          in(f x' , s'))

instance Applicative S where
  pure :: a -> S a
  pure x = S(\s -> (x, s))

  (<*>) :: S (a -> b) -> S a -> S b
  (<*>) (S f) (S x) = S(\s -> let (x', s') = x s
                                  (f', s'') = f s'
                              in(f' x' , s''))


instance Monad S where
  return :: a -> S a
  return x = S(\s -> (x, s))

  (>>=) :: S a -> (a -> S b) -> S b
  (>>=) (S x) f = S(\s -> let
                            (x', s') = x s
                            S g = f x'
                          in g s')