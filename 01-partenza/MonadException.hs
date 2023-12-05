{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant lambda" #-}

module MonadException where


-- Passando da EvalBasic.hs a EvalException.hs 
-- abbiamo esteso l'interprete:
--
--       EvalBasic.ev:: Term -> BN
--
-- ad un interprete:
--
--       EvalException.ev:: Term -> E BN 
--
-- che, oltre a produrre il risultato di tipo BN 
-- di EvalBasic.ev:: Term -> BN produce anche qualche 
-- ulteriore effetto permesso dal costruttore E.
--
-- La lettura di:
--
--      Term -> E BN
--
-- può essere:
--
--      Preso un valore in Term, la funzione
--      produce un valore in BN, assieme a un
--      qualche effetto collaterale descritto
--      da E.


type Exception = String
data E a where
    Rise   :: Exception -> E a
    Return :: a -> E a
    deriving (Show)

instance Functor E where
  fmap :: (a -> b) -> E a -> E b
  ...

instance Applicative E where 
  pure :: a -> E a
  ...

  (<*>) :: E (a -> b) -> E a -> E b
  ...

instance Monad E where
  (>>=) :: E a -> (a -> E b) -> E b
  ...
  return :: a -> E a
  ...




