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


mul:: Positive -> Positive -> Positive
mul XH y     = y
mul (XO x) y = XO (mul x y)
mul (XI x) y = add y (XO (mul x y))

-- propedeutico alla definizione di sub
data Mask where
  IsNeg :: Mask
  IsNul :: Mask
  IsPos :: Positive -> Mask
  deriving (Show, Eq) -- necessario per testing (positive need eq)


sub :: Positive -> Positive -> Mask
sub XH XH       = IsNul
sub XH _        = IsNeg
sub m XH       = IsPos (pred m)
-- Induttivo
sub (XI x) (XO y) = case sub x y of 
                      IsNeg -> IsNeg 
                      IsNul -> IsPos XH
                      (IsPos d) -> IsPos (XI d)
sub (XO x) (XI y) = case sub x y of 
                      IsNeg -> IsNeg 
                      IsNul -> IsNeg
                      (IsPos d) -> IsPos (pred (XO d))
sub (XO x) (XO y) = case sub x y of 
                      IsNeg -> IsNeg 
                      IsNul -> IsNul
                      (IsPos d) -> IsPos (XO d)
sub (XI x) (XI y) = case sub x y of 
                      IsNeg -> IsNeg 
                      IsNul -> IsNul
                      (IsPos d) -> IsPos (XO d)

