module Task4_2 where

data FourOf a = FourOf a a a a deriving(Show,Eq)

-- реализуйте классы `Functor`, `Applicative` и `Monad` для типа `FourOf`
-- таким образом, что 
-- do { x <- FourOf 1 2 3 4; y <- FourOf 4 6 7 8; return $ x + y } === FourOf 5 8 10 12

instance Functor (FourOf) where
    fmap f (FourOf a b c d) = FourOf (f a) (f b) (f c) (f d)

instance Monad FourOf where
    return a = FourOf a a a a
    (>>=) (FourOf a b c d) f  = FourOf (num1 (f a)) (num2 (f b)) (num3 (f c)) (num4 (f d))

instance Applicative FourOf where
    pure = return
    (<*>) (FourOf a b c d) (FourOf a1 b2 c3 d4) = FourOf (a a1) (b b2) (c c3) (d d4)

num1 (FourOf a _ _ _) = a
num2 (FourOf _ a _ _) = a
num3 (FourOf _ _ a _) = a
num4 (FourOf _ _ _ a) = a
