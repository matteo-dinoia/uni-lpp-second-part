{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Redundant bracket" #-}

module EvalBasicIDMonadic where

import BNumFuncApp ( BN, BinNotation ( .. ) )
import qualified BNumFuncApp as B
import BNumFuncAppVsInteger ( bN2Int, int2BN )
import Language ( Term ( .. ) )
import MonadID ( ID )
import qualified MonadID as MID


ev :: Term -> MID.ID BN
ev (Tcon v)   = return v;
ev (Tdiv a b) = (ev a) >>=
                    \va -> ((ev b) >>=
                        \vb -> return (B.div va vb))

-- TEST
ok , nok :: Term
ok  = Tdiv (Tdiv (Tcon (int2BN 1972))
                 (Tcon (int2BN 2   )))
           (Tcon (int2BN 23))
nok = Tdiv (Tcon (int2BN 1)) (Tcon (int2BN 0))

test :: IO ( )
test = do
    print "-------------- EvalBasicMonadic.ev"
    let (MID.Id evok) = ev ok
    print (bN2Int evok)
    -- let (MID.Id evnok) = evCanonico nok
    -- print (bN2Int evnok)