{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# HLINT ignore "Use tuple-section" #-}
{-# HLINT ignore "Avoid lambda" #-}

module EvalStatePureMonadic where

import BNumFuncApp ( BN, BinNotation ( .. ) )
import qualified BNumFuncApp as B
import BNumFuncAppVsInteger ( bN2Int, int2BN )
import Language ( Term ( .. ) )
import MonadStatePure ( State, S ( .. ) )
import qualified Data.Functor

ev :: Term -> S BN --( s -> (a, s'))
ev (Tcon v)   = return v;
ev (Tdiv ta tb) = (ev ta) >>=
                        \a -> ((ev tb) >>=
                            \b -> S(\s -> (B.div a b, s)))


-- TEST
ok , nok :: Term
ok  = Tdiv (Tdiv (Tcon (int2BN 1972))
                 (Tcon (int2BN 2   )))
           (Tcon (int2BN 23))
nok = Tdiv (Tcon (int2BN 1)) (Tcon (int2BN 0))

test :: IO ( )
test = do
    print "-------------- EvalStateIDMonadic.hs"
    print "-------------- ev"
    let S f = ev ok
    let (out, s) = f 0
    print (bN2Int out, s)

    let S f = ev ok
    let (out, s) = f 5
    print (bN2Int out, s)

    let S f = ev nok
    let (out, s) = f 5
    print (out, s)
