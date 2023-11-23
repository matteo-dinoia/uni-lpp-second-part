module BNum where

import Prelude hiding (succ, pred)
import PositiveNum ( Mask( .. ), Positive( .. ), succ, add, pred, sub, mul )
import qualified PositiveNum as P
import PositiveNumVsInteger ( int2Pos )

-- numeri naturali in notazione binaria
data BN where
  N0 :: BN
  Npos :: Positive  -> BN
  deriving Show


-- funzioni unarie
succ :: BN -> BN
succ N0 = Npos XH
succ (Npos x) = Npos (PositiveNum.succ x)

pred :: BN -> BN
pred N0        = N0
pred (Npos XH) = N0
pred (Npos x)  = Npos (PositiveNum.pred x)

-- funzioni binarie
add :: BN -> BN -> BN
add N0 x              = x
add x N0              = x
add (Npos x) (Npos y) = Npos (PositiveNum.add x y)

sub :: BN -> BN -> BN
sub x N0 = x
sub N0 x = N0
sub (Npos x) (Npos y) = case PositiveNum.sub x y of
                          IsPos d -> Npos d
                          _ -> N0

mul:: BN -> BN -> BN
mul N0 x              = N0
mul x N0              = N0
mul (Npos x) (Npos y) = Npos (PositiveNum.mul x y)


-- altre funzioni (per estendere Num)
negate :: BN -> BN
negate x = x -- assumo faccia nulla

abs :: BN -> BN
abs x = x

signum :: BN -> BN
signum N0 = N0
signum _  = Npos XH

fromInteger :: Integer -> BN
fromInteger n | n <= 0    = N0
              | otherwise = Npos(int2Pos n)

-- necessario per il testing
instance Eq BN where
  (==) N0 N0             = True
  (==) N0 _              = False
  (==) _ N0              = False
  (==) (Npos x) (Npos y) = (==) x y
  (/=) x y = not ((==) x y)
