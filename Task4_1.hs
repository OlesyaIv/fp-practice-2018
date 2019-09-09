module Task4_1 where
-- Монада над функцией. В качестве входного значения `fun` может быть что угодно
-- Собственно, почему бы не `String`?
data FunMonad a = FunMonad { fun :: String -> a }

-- реализуйте классы `Functor`, `Applicative` и `Monad` для типа `FunMonad`

instance Functor FunMonad where
    fmap b (FunMonad a) = FunMonad (b . a)

instance Applicative FunMonad where
    pure a = FunMonad $ \x -> a
    (<*>) (FunMonad a) (FunMonad b) = FunMonad $ \x -> (a x) (b x)

instance Monad FunMonad where
    return = pure
    (>>=) (FunMonad a) b = FunMonad $ \x -> (fun $ b $ a x) x