{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module BNumFuncApp where

import Prelude hiding (succ, pred, div)
import PositiveNum( Mask( .. ), Positive( .. ), succ, add, pred, sub, mul)
import qualified PositiveNum as PN
import PositiveNumVsInteger ( int2Pos )
import qualified PositiveNumVsInteger as PNI
-- import PositiveNumOrder ( )
import GHC.Base ( Applicative ( .. ) )

-- Notazione che include lo "zero" per un qualche insieme
-- di numeri positivi.
data BinNotation a where
  N0   :: BinNotation a
  Npos :: a -> BinNotation a
  deriving (Show)

instance Functor BinNotation where
  --fmap :: (a -> b) -> BinNotation a -> BinNotation b
  fmap f N0 = N0
  fmap f (Npos a) = Npos (f a)


-- Numeri naturali in notazione binaria a partire
-- dai numeri positivi.
type BN = BinNotation Positive

-- Funzioni unarie
succ :: BN -> BN
succ N0 = Npos PN.XH
succ x  = fmap PN.succ x

pred :: BN -> BN
pred N0           = N0
pred (Npos PN.XH) = N0
pred x            = fmap PN.pred x

instance Applicative BinNotation where
  pure a = Npos a
  --liftA2 :: (a -> b -> c) -> BinNotation a -> BinNotation b -> BinNotation c
  liftA2 f N0 N0             = N0
  liftA2 f N0 _              = N0
  liftA2 f _ N0              = N0
  liftA2 f (Npos a) (Npos b) = Npos (f a b)

  (<*>) N0 fa = N0
  (<*>) (Npos f) fa = fmap f fa


-- Funzioni binarie
-- Due addizione alternative
add :: BN -> BN -> BN
add x y = (fmap PN.add x) <*> y

add' :: BN -> BN -> BN
add' N0 b = b
add' a N0 = a
add' a b  = liftA2 PN.add a b

sub :: BN -> BN -> BN
sub a b  = case liftA2 PN.sub a b  of
            (Npos (IsPos p)) -> Npos p
            _                -> N0


mul:: BN -> BN -> BN
mul x y = (fmap PN.mul x) <*> y

instance Num BN where
  (+) = BNumFuncApp.add
  (*) = BNumFuncApp.mul
  (-) = BNumFuncApp.sub
  abs x = x

  signum N0 = N0
  signum x = Npos PN.XH

  fromInteger 0 = N0
  fromInteger x = Npos (int2Pos x)

-- instance Eq BN where

-- instance Ord BN where

-- div :: BN -> BN -> BN
