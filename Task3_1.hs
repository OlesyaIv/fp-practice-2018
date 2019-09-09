module Task3_1 where

data WeirdPeanoNumber = Zero | Succ WeirdPeanoNumber | Pred WeirdPeanoNumber

-- Реализуйте все классы типов, которым должны отвечать целые числа

normalize :: WeirdPeanoNumber -> WeirdPeanoNumber
normalize Zero = Zero
normalize (Succ (Pred a)) = normalize a
normalize (Pred (Succ a)) = normalize a
normalize (Succ a) = Succ $ normalize a
normalize (Pred a) = Pred $ normalize a

toInt :: WeirdPeanoNumber -> Integer
toInt (Succ a) = (toInt a) + 1
toInt (Pred a) = (toInt a) - 1
toInt Zero = 0
 
fromInt :: Integer -> WeirdPeanoNumber
fromInt a
    | a < 0 = Pred $ fromInt (a + 1)
    | a > 0 = Succ $ fromInt (a - 1)
    | otherwise = Zero

instance Eq WeirdPeanoNumber where
  (==) a b = cmprEq (normalize a) (normalize b)
  (/=) a b = not (a == b)

cmprEq :: WeirdPeanoNumber -> WeirdPeanoNumber -> Bool
cmprEq Zero Zero = True
cmprEq Zero _ = False
cmprEq _ Zero = False
cmprEq (Succ a) (Pred b) = False
cmprEq (Pred a) (Succ b) = False
cmprEq (Succ a) (Succ b) = a `cmprEq` b
cmprEq (Pred a) (Pred b) = a `cmprEq` b
cmprEq _ _ = False

instance Ord WeirdPeanoNumber where
  compare a b = cmprOrd (normalize a) (normalize b)

cmprOrd :: WeirdPeanoNumber -> WeirdPeanoNumber -> Ordering
cmprOrd Zero Zero = EQ
cmprOrd Zero (Succ _) = LT
cmprOrd Zero (Pred _) = GT
cmprOrd (Succ _) Zero = GT
cmprOrd (Pred _) Zero = LT
cmprOrd (Succ a) _ = GT
cmprOrd (Pred a) _ = LT
cmprOrd (Succ a) (Pred b) = GT
cmprOrd (Pred a) (Succ b) = LT
cmprOrd (Succ a) (Succ b) = a `cmprOrd` b
cmprOrd (Pred a) (Pred b) = a `cmprOrd` b

instance Num WeirdPeanoNumber where
    (+) a Zero = a
    (+) Zero a = a 
    (+) a b = fromInt (toInt a + toInt b)

    (*) _ Zero = Zero
    (*) Zero _ = Zero
    (*) a b = fromInt (toInt a * toInt b)

    negate Zero = Zero
    negate (Succ a) = Pred (negate a)
    negate (Pred a) = Succ (negate a) 

    signum Zero = Zero
    signum (Succ (Pred a)) = signum a
    signum (Pred (Succ a)) = signum a
    signum (Succ a) = Succ Zero
    signum (Pred a) = Pred Zero

    abs a = if (compare a 0 == LT) then negate a else a

    fromInteger a = fromInt (fromIntegral a)

instance Enum WeirdPeanoNumber where
    fromEnum (Succ a) = (fromEnum a) + 1
    fromEnum (Pred a) = (fromEnum a) - 1
    fromEnum Zero = 0

    toEnum a
        | a < 0 = Pred $ toEnum (a + 1)
        | a > 0 = Succ $ toEnum (a - 1)
        | otherwise = Zero

instance Real WeirdPeanoNumber where
    toRational a = toRational (toInt a)

instance Integral WeirdPeanoNumber where
    quotRem a b 
        | (signum a) == (signum b) = (quot a b, (normalize a) - a * (rem a b))
        | b == Zero = error "Division by ZERO"
        | otherwise = (negate (quot a b), signum a * (rem a b))

quot' a b 
    | a < b = Zero
    | otherwise = (quot (a-b) b) + 1 
   where 
        a = normalize (abs a)
        b = normalize (abs b)

rem' a b 
    | a < b = a
    | a == b = Zero
    | otherwise = rem (a - b) b
   where 
        a = normalize (abs a)
        b = normalize (abs b)


instance Show WeirdPeanoNumber where
    show Zero = "Zero"
    show (Succ a) = "(Succ " ++ show a ++ ")"
    show (Pred a) = "(Pred " ++ show a ++ ")"