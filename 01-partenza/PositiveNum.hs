module PositiveNum where
import Prelude hiding (succ, pred)

-- numeri positivi in notazione binaria
data Positive where
  XH :: Positive              -- Partenza = 1
  XO :: Positive -> Positive  -- n => 2n        ... => ...0
  XI :: Positive -> Positive  -- n => 2n +1     ... => ...1
  deriving Show

-- trovo info con :i eq
instance Eq Positive where  -- mi serve poterli confrontare
  (==) XH XH         = True
  (==) (XO x) (XO y) = (==) x y
  (==) (XI x) (XI y) = (==) x y
  (==) _ _           = False
  (/=) x y = not ((==) x y)



-- funzioni unarie
succ :: Positive -> Positive
succ XH     = XO(XH)
succ (XO x) = XI x
succ (XI x) = XO (succ x)

pred :: Positive -> Positive
pred XH      = XH
pred (XO XH) = XH
pred (XI x)  = XO x
pred (XO x)  = XI (pred x)

-- funzioni binarie
add :: Positive -> Positive -> Positive
add XH x = succ x
add x XH = succ x
add (XO x) (XO y) = XO (add x y)
add (XO x) (XI y) = XI (add x y)
add (XI x) (XO y) = XI (add x y)
add (XI x) (XI y) = XO (succ (add x y))

-- casi base
{-
-- casi induttivi
-- 2m+1 + 1 == ... 
-- 2m + 2n == ... 
-- 2m + 2n+1 == ...
-- 2m+1 + 2n+1 == ...
-- 2m+1 + 2n == ...

mul:: Positive -> Positive -> Positive
...



-- propedeutico alla definizione di sub
data Mask where
  IsNeg :: Mask
  IsNul :: Mask
  IsPos :: Positive -> Mask
  deriving (Show, 
            Eq) -- necessario per testing

sub :: Positive -> Positive -> Mask
...
-- tre casi base

-- casi induttivi
-- 2m - 2n == ...
-- (2m+1) - 2n == ...
-- (2m) - (2n+1) == ...
-- (2m+1) - (2n+1) == ...
-}