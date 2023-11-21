module BNum where

import Prelude hiding (succ, pred)
import PositiveNum ( Mask( .. ), Positive( .. ), succ, add, pred, sub, mul )
import qualified PositiveNum as P
import PositiveNumVsInteger ( int2Pos )

-- numeri naturali in notazione binaria
data BN where
  N0 :: BN
  Npos :: Positive  → BN
  deriving Show

{-
-- funzioni unarie
succ :: BN -> BN
  ...

pred :: BN -> BN
  ...

-- funzioni binarie
add :: BN -> BN -> BN
  ...
-- due casi base
-- un caso "induttivo"


sub :: BN -> BN -> BN
  ...
-- due casi base
-- un caso "induttivo" che schiaccia a zero

mul:: BN -> BN -> BN
  ...
-- due casi base
-- un caso "induttivo"

-- altre funzioni
negate :: BN -> BN
negate x = x

abs :: BN -> BN
abs x = x

signum :: BN -> BN
signum N0 = ...
signum _  = ...

fromInteger :: Integer -> BN
fromInteger n | n <= 0    = ...
              | otherwise = ...

-- necessario per il testing
instance Eq BN where
  (==) :: BN -> BN -> Bool
    ...
  (/=) :: BN -> BN -> Bool
    ...
-}