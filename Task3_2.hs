module Task3_2 where

import Todo(todo)

data ReverseList a = RNil | RCons (ReverseList a) a

rlistToList :: ReverseList a -> [a]
rlistToList a = rlistToList' a [] where
                rlistToList' RNil lst = lst;
                rlistToList' (RCons t h) lst = rlistToList' t (h:lst)

listToRList :: [a] -> ReverseList a
listToRList a = listToRList' a RNil where
                listToRList' [] lst = lst;
                listToRList' (h:t) lst = listToRList' t (RCons lst h)

-- Реализуйте классы Eq, Ord, Show, Monoid, Functor

instance (Eq a) => Eq (ReverseList a) where
    (==) RNil RNil = True
    (==) _    RNil = False
    (==) RNil _    = False
    (==) (RCons t h) (RCons t' h') = h == h' && t == t'

instance (Ord a) => Ord (ReverseList a) where
    (<=) RNil RNil = True
    (<=) RNil _ = True
    (<=) _ RNil = False
    (<=) (RCons t h) (RCons t' h') = if (h <= h') then True
                                      else t <= t'

instance (Show a) => Show (ReverseList a) where
    show RNil = "R[]"
    show (RCons RNil t) = "R[" ++ show t ++ "]"
    show t1 = "R[" ++ showRList t1 ++ "]" where 
        showRList (RCons RNil h) = show h
        showRList (RCons t h) = showRList t ++ (',' : show h)

instance Semigroup (ReverseList a) where
    (<>) t t' = sgroup t' t where
                sgroup RNil t = t;
                sgroup t RNil = t
                mappend k (RCons t h) = RCons (mappend k t) h

instance Monoid (ReverseList a) where
    mempty = RNil
    mappend = (<>)

instance Functor ReverseList where
    fmap f RNil = RNil
    fmap f (RCons t h) = RCons (fmap f t) (f h)