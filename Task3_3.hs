module Task3_3 where

newtype PSet a = PSet{ contains :: (a -> Bool) }
newtype PSet' a = PSet'(a -> Bool) 
newtype PSet1 a = PSet1(a -> Bool) 

-- Реализуйте классы Monoid и Functor
-- Объясните в комментариях, почему они реализованы именно так


-- Моноид:
-- логическое ИЛИ и нейтральное значения False, а именно
-- False || a === a || False === а
-- Либо же для логического И и нейтрального значения True, а именно
-- True && a === a && True === а
-- Исключающее ИЛИ и нейтральное значение False, а именно
-- True && a === a && True === а
-- все варианты отвечают нейтральному значению

-- False ||
instance Semigroup (PSet x) where
    (<>) (PSet a) (PSet b) = PSet (\x -> (a x) || (b x))

instance Monoid (PSet x) where
  mempty = PSet (\x -> False)
  mappend = (<>)

-- True &&
instance Semigroup (PSet' x) where
    (<>) (PSet' a) (PSet' b) = PSet' (\x -> (a x) && (b x))

instance Monoid (PSet' x) where
  mempty = PSet' (\x -> True)
  mappend = (<>)

-- False Исключающее ||
instance Semigroup (PSet1 x) where
    (<>) (PSet1 a) (PSet1 b) = PSet1 (\x -> a x && not(b x) || not(a x) && b x)

instance Monoid (PSet1 x) where
  mempty = PSet1 (\x -> False)
  mappend = (<>)

-- Функтор:
-- Результатом будет False, т.к. мы знаем только отображение из множества b в a
-- А именно fmap (a -> b) -> PSet a -> PSet b работать не будет
-- Однако можно использовать контрвариантный функтор - отображение, обращающее стрелки
-- и сохраняющее тождественные морфизмы, т.е. в целом меняющий порядок наоборот
-- Что преобразует выражение выше в аьфз (a -> b) -> PSet b -> PSet a

class NewFunctor f where
    fmap1 :: (a -> b) -> f b -> f a

instance NewFunctor PSet where
    fmap1 f (PSet a) = PSet (a . f)