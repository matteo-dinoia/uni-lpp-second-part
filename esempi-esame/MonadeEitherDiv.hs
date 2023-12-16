-- PREMESSA
-- Either è una monade Haskell usata per rappresentare
-- computazioni che possono restituire un risultato
-- o un errore:
--
-- data Either a b where
--      Left  :: a -> Either a b
--      Right :: b -> Either a b
--
-- instance Monad (Either e) where
--     return = Right
--     Left l >>= _ = Left l
--     Right r >>= k = k r
--

-- DOMANDA 1
-- Scrivere almeno una versione di funzione divSicuraE che, sfruttando la monade
-- Either, soddisfa le seguenti caratteristiche:
--
-- A. divSicura ha due Float in input
--
-- B. divSicura restituisce un Float ottenuto come divisione del primo argomento
-- per il secondo, se il secondo argomento non è zero.
--
-- C. divSicura restituisce il messaggio "Div by zero" se il secondo argomento
-- è zero.

type Error = String

divSicura :: Float -> Float -> Either Float Error
divSicura _ 0 = Right "Div by zero"
divSicura a b = Left (a / b);

-- DOMANDA 2
-- Scrivere una funzione reciprocaSicura con le seguenti caratteristiche
-- A. reciprocaSicura ha un Float in input
--
-- B. reciprocaSicura restituisce un Float ottenuto come reciproco ("uno su ...")
-- dell'argomento se non è zero.
--
-- C. reciprocaSicura restituisce il messaggio "Div by zero" se l'argomento
-- è zero.

reciprocaSicura :: Float -> Either Float Error
reciprocaSicura = divSicura 1