module EvalStateEsercizioVariabili where
import BNumFuncApp ( BN, BinNotation ( .. ) )
import qualified BNumFuncApp as B
import BNumFuncAppVsInteger ( bN2Int, int2BN )
import HelperTest ( terneFXY, errListFXY )



-- Linguaggio di termini Term aggiornato per poter
-- usare variabili identificate da un nome, che è
-- l'argomento del costruttore Tv.
--
-- 1ma IPOTESI. Un identificatore è un singolo
-- carattere.
data Term' where
    Tv :: Char -> Term'
    Tc :: BN -> Term'
    Td :: Term' -> Term' -> Term'
    deriving (Show)

-- 2d IPOTESI. Uno stato è un "dizionario":
-- lista di coppie ('carattere', b)::((,) Char BN)
type State' = [(char , BN)]

-- Dati un nome di variabile e uno stato "dizionario"
-- è utile definire una funzione che restituisce il
-- valore della variabile data.
--
-- Cominciare con una definizione di funzione che,
-- anche in caso di insuccesso, restituisce un valore
-- di tipo BN.
look :: Char -> State' -> BN  -- a -> [(a , b)] -> b
look c [] = N0
look c ((sc, sn) :: ss)  | c == sc     = sn
                         | otherwise   = look c ss

-- Il costruttore di tipo del risultato fornito
-- dall'interprete dei termini in Term' permette di
-- calcolare il valore di termini in cui compaiono
-- variabili il cui valore dipende dallo stato di
-- valutazione.
type S' a = (->) State' ((,) a State') -- State1 -> (a , State1)

-- Segue l'interprete da Term' a S' BN in grado di
-- usare lo stato per stabilire il valore da associare
-- alle variabili di un termine da valutare.
evEx :: Term' -> S' BN
evEx (Tv c) s     = (look c s , s)
evEx (Tc n) s     = (n , s)
evEx (Td t1 t2) s = (B.div t1 t2 , s)

-- ESTENSIONI
--
-- 1.   Gestire l'insuccesso di look con un tipo adeguato.
-- 2.   Estendere il linguaggio con un costruttore Ta da
--      interpretare come assegnazione che altera lo stato,
--      o modificando il valore associato a una variabile o
--      estendendo lo stato
--