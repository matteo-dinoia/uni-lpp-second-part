{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use tuple-section" #-}
{-# HLINT ignore "Redundant lambda" #-}
{-# HLINT ignore "Use camelCase" #-}

module MonadStateID where
import MonadID ( ID  ( .. ) )


type State = Integer
newtype S a where
  ID :: (State -> (a, State)) -> S a

instance Functor S where
  fmap :: (a -> b) -> S a -> S b
  ...

instance Applicative S where
  pure :: a -> S a
  ...

  (<*>) :: S (a -> b) -> S a -> S b
  ...

instance Monad S where
  return :: a -> S a
  ...

  (>>=) :: S a -> (a -> S b) -> S b
  ...
