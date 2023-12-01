module EvalState where

import BNumFuncApp ( BN, BinNotation ( .. ) )
import qualified BNumFuncApp as B
import BNumFuncAppVsInteger ( bN2Int, int2BN )
import HelperTest ( terneFXY, errListFXY )
import Language ( Term ( .. ) )

type State = Int
-- L'operatore '->' è il costruttore
type S a = State -> (a, State)


ev :: Term -> S BN -- Term -> (Int -> (BN, Int) )
ev (Tcon b)   = \s -> (b, s + 1)
ev (Tdiv n d) = \s ->
        let (bn , sn) = ev n s
            (bd , sd) = ev d sn
        in
            (B.div dn bd , sd)  -- permette di contare elementi


-- TEST
ok , nok :: Term
ok  = Tdiv (Tdiv (Tcon (int2BN 1972))
                 (Tcon (int2BN 2   )))
           (Tcon (int2BN 23))
nok = Tdiv (Tcon (int2BN 1)) (Tcon (int2BN 0))

test :: IO ( )
test = do

   print "-------------- EvalState.ev"
   print (ev ok 0) -- con 0 offset del conteggio
   print (ev ok 3) -- con 3 offset del conteggio
   -- print (ev nok 0) -- stack overflow
