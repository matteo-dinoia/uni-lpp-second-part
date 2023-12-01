{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use tuple-section" #-}

module EvalExceptionEsercizioConStatoVariabili where

import BNumFuncApp ( BN, BinNotation ( .. ) )
import qualified BNumFuncApp as B
import BNumFuncAppVsInteger ( bN2Int, int2BN )
import HelperTest ( terneFXY, errListFXY )

data Term where
    Tv :: Char -> Term
    Tc :: BN -> Term
    Td :: Term -> Term -> Term
    deriving (Show)

type Exception = String
data E a where
    Rise   :: Exception -> E a
    Return :: a -> E a
    deriving (Show)

type State = [(Char, BN)]

look :: Char -> State -> E BN
look n []         = Rise "Variable not found"
look n ((k,v):s') | n == k    = Return v
                  | otherwise = look n s'

type S a = State -> (a , State)

ev :: Term -> S (E BN)
ev (Tv c) s = (look c s, s)
ev (Tc n) s = (Return n , s)
ev (Td t1 t2) s =
                let
                    (v1, s1) = ev t2 s
                    (v2, s2)= ev t2 s1
                in
                    case (v1 , v2) of
                        (Rise t , _) -> (Rise t, s2)
                        (_ , Rise t) -> (Rise t, s2)
                        (Return w1, Return N0) -> (Rise "Div by 0" , s2)
                        (Return w1, Return w2) -> (Return (B.div w1 w2) , s2)


-- TEST
s :: [(Char,BN)]
s = [('a', int2BN 1972), ('b', 2), ('c', int2BN 1)]
ok , nok :: Term
-- (1972/2)/23
ok  = Td (Td (Tv 'a')
             (Tv 'b'))
         (Tc (int2BN 23))
-- 1/0
nok = Td (Tv 'c')
         (Tc (int2BN 0))

test :: IO ( )
test = do
    print "-------------- EvalException.ev"
    print (ev ok  s)
    print (ev nok s) -- "Div by 0"
