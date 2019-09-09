module Task2_2 where

import Todo(todo)

import Prelude hiding (foldl, foldr, unfoldr, map, concatMap, 
    filter, maxBy, minBy, reverse, sum, product, elem)

foldl :: (b -> a -> b) -> b -> [a] -> b
foldl f x [] = x
foldl f x (h : t) = foldl f (f x h) t

foldr :: (a -> b -> b) -> b -> [a] -> b
foldr f x [] = x
foldr f x (h : t) = f h (foldr f x t)

unfoldr :: (b -> Maybe (a, b)) -> b -> [a]
unfoldr f x = case f x of
               Just (a, x') -> a : unfoldr f x'
               Nothing -> []

-- Сумма всех элементов списка (пример)
sum :: [Integer] -> Integer
sum lst = foldl (+) 0 lst

-- Переворот списка (Пример)
reverse :: [a] -> [a]
reverse [] = []
reverse lst = foldl f [] lst where f t h = h:t

-- Отображение элементов списка
map :: (a -> b) -> [a] -> [b]
map _ [] = []
map f lst = foldr (\h t -> (f h):t) [] lst

-- Произведение всех элементов списка
product :: [Integer] -> Integer
product lst = foldl (*) 1 lst

-- Выделение из списка Maybe всех существующих значений
catMaybes :: [Maybe a] -> [a]
catMaybes lst = foldr existVal [] lst
                 where existVal a lst = case a of
                                           Just v -> (v: lst)
                                           Nothing -> lst

-- Диагональ матрицы
diagonal :: [[a]] -> [a]
diagonal matr = unfoldr f 0 
            where f m
                    | (m < sizeList matr) = Just(((matr !! (fromIntegral m)) !! (fromIntegral m)), m + 1)
                    | otherwise = Nothing
                   where sizeList lst = foldl f 0 lst 
                                where f acc element = acc + 1

-- Фильтр для всех элементов, не соответствующих предикату
filterNot :: (a -> Bool) -> [a] -> [a]
filterNot m lst = foldr (getNum m) [] lst
                     where getNum m a lst 
                                    | (m a) = a : lst
                                    | otherwise = lst
 
-- Поиск элемента в списке
elem :: (Eq a) => a -> [a] -> Bool
elem m lst = (filterNot ( == m) lst) /= []

-- Список чисел в диапазоне [from, to) с шагом step
rangeTo :: Integer -> Integer -> Integer -> [Integer]
rangeTo from to step = unfoldr f from 
                         where f m 
                                | m < to = Just (m, m + step)
                                | otherwise = Nothing

-- Конкатенация двух списков
append :: [a] -> [a] -> [a]
append [] b = b
append a [] = a
append a b = foldr f b a 
           where f t h = t:h

-- Разбиение списка lst на куски размером n
-- (последний кусок может быть меньше)
groups :: [a] -> Integer -> [[a]]
groups t n = unfoldr f t where
             f [] = Nothing
             f m = let snd' = snd(splitList n m)
                       fst' = fst(splitList n m) in
                   if (sizeList snd' == 0) && (sizeList fst' == 0) then Nothing
                   else Just (fst',snd')
                  where 
                        splitList 0 t = ([], t);
                        splitList _ [] = ([], []);
                        splitList n (h:t) | n < 0 = ([], (h:t))
                                          | otherwise = ((h:a), b)
                                          where (a, b) = splitList (n - 1) t;
                        sizeList lst = foldl f 0 lst 
                                where f acc element = acc + 1


