{-# LANGUAGE InstanceSigs #-}

-- Sia data la seguente definizione di tipo di dato:

data BW a where
    BW :: (a, a) -> BW a

-- DOMANDA 1
-- costruire una istanza di Functor con BW
instance Functor BW where
    fmap :: (a -> b) -> BW a -> BW b
    fmap f (BW (a , b)) = BW (f a , f b)


-- DOMANDA 2
-- costruire una istanza di Applicative con BW
instance Applicative BW where
    pure :: a -> BW a
    pure a = BW (a , a)
    (<*>) :: BW (a -> b) -> BW a -> BW b
    (<*>) (BW (f , g)) (BW (a , b)) = BW (f a , g b)

-- DOMANDA 3
-- costruire una istanza di Monad con BW
instance Monad BW where
    return :: a -> BW a
    return a = BW (a , a)
    (>>=) :: BW a -> (a -> BW b) -> BW b
    (>>=) (BW (a, b)) f = f a

-- DOMANDA 4
-- È possibile immaginare più definizioni della
-- funzione bind, rispondendo alla DOMANDA 3?
-- In caso positivo, fornirne una.
bind2 :: BW a -> (a -> BW b) -> BW b
bind2 (BW (a, b)) f =  f a >>= (\ap -> f b >>= \bp -> BW (ap, bp))