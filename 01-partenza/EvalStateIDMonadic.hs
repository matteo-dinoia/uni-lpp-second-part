{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# HLINT ignore "Use tuple-section" #-}
{-# HLINT ignore "Avoid lambda" #-}

module EvalStateIDMonadic where

import BNumFuncApp ( BN, BinNotation ( .. ) )
import qualified BNumFuncApp as B
import BNumFuncAppVsInteger ( bN2Int, int2BN )
import Language ( Term ( .. ) )
import MonadStateID ( State, S ( .. ) )
import qualified Data.Functor

ev :: Term -> S BN
...

-- Evitando di "ignorare i lambda", usando
-- 
-- {-# HLINT ignore "Avoid lambda" #-}
-- 
-- si ottiene un primo suggerimento:
--
--     Avoid lambda
--     Found:
--     \b -> return (B.div a b)
--     Why not:
--     return . B.div a
-- 
-- che è seguito da un secondo suggerimento
-- che invita a usare la funzione applicazione
-- <&>, producendo:

evOttimizzato :: Term -> S BN
evOttimizzato (Tcon b) = return b
evOttimizzato (Tdiv t t') =
   (evOttimizzato t)
   >>=
   (\a -> evOttimizzato t' Data.Functor.<&> B.div a)

-- È evidente il vantaggio di aver inserito lo sviluppo
-- nel quadro dell'infrastruttura offerta da Haskell. 
-- Il "vantaggio", ammesso che lo sia, è una maggiore
-- compattezza del codice.
-- La definizione qui sopra, offerta da HLS è basata
-- sul fatto che una monade è un funtore: 
-- vedere il commento a linea 20 del modulo MonadStateId.hs

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
    let ID f = ev ok
    let (out, s) = f 0
    print (bN2Int out, s)

    let ID f = ev ok
    let (out, s) = f 5
    print (bN2Int out, s)

    let ID f = ev nok
    let (out, s) = f 5
    print (out, s)

    print "-------------- evInOttimizzato"
    let ID f = evOttimizzato ok
    let (out, s) = f 0
    print (bN2Int out, s)

   --  let ID f = evOttimizzato ok
   --  let (out, s) = f 5
   --  print (bN2Int out, s)