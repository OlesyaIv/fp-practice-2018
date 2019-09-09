module Task2_1 where

import Todo(todo)
import Prelude hiding(lookup)
-- Ассоциативный массив на основе бинарного дерева поиска
-- Ключи - Integer, значения - произвольного типа
data TreeMap v = EmptyTree
                | Node Integer v (TreeMap v) (TreeMap v) 

-- Пустое дерево
emptyTree :: TreeMap v
emptyTree = EmptyTree

-- Содержится ли заданный ключ в дереве?
contains :: TreeMap v -> Integer -> Bool
contains EmptyTree _ = False
contains (Node key _ left right) k
    | k == key = True
    | k > key = contains right k
    | k < key = contains left k

-- Значение для заданного ключа
lookup :: Integer -> TreeMap v -> v
lookup k (Node key val left right )
    | k == key = val
    | k > key = lookup k right
    | k < key = lookup k left

-- Вставка пары (ключ, значение) в дерево
insert :: (Integer, v) -> TreeMap v -> TreeMap v
insert (k, v) (Node key val left right)
    | k == key = Node k v left right
    | k > key = Node key val left (insert (k,v) right)
    | k < key = Node key val (insert (k,v) left) right

-- Удаление элемента по ключу         

delLeftNode :: TreeMap v -> (Integer, v)
delLeftNode (Node key val EmptyTree _) = (key, val)
delLeftNode (Node key val l _) = delLeftNode l

delLeft :: TreeMap v -> TreeMap v -> TreeMap v
delLeft l r = let (k, v) = delLeftNode r in Node k v l (remove k r) 

remove :: Integer -> TreeMap v -> TreeMap v
remove _ EmptyTree = error "Empty"
remove i (Node key val left right) | i > key = remove i right
                              | i < key = remove i left
                              | i == key = case (left, right) of
                                            (EmptyTree, EmptyTree) -> EmptyTree
                                            (left, EmptyTree) -> left
                                            (EmptyTree, right) -> right
                                            (left, right) -> delLeft left right

-- Поиск ближайшего снизу ключа относительно заданного
nearestLE :: Integer -> TreeMap v -> (Integer, v)
nearestLE k EmptyTree = error "Empty"
nearestLE k node@(Node key val left right) | k > key = nearestLE k right
                                           | k < key = nearestLE k left
                                           | k == key = minKey node
                                          where
                                             minKey (Node key value EmptyTree right) = getKeyVal right
                                             minKey (Node key value left EmptyTree) = getKeyVal left
                                             minKey (Node key value EmptyTree EmptyTree) = (key, value)
                                             minKey (Node key value l r) | abs'((key - getKey l)) <= abs'((key -getKey r)) = getKeyVal l
                                                                         | otherwise = getKeyVal r

getKey :: TreeMap v-> Integer
getKey (Node key _ _ _) = key
getKeyVal :: TreeMap v-> (Integer, v)
getKeyVal (Node key value _ _) = (key, value)

-- Построение дерева из списка пар
treeFromList :: [(Integer, v)] -> TreeMap v
treeFromList lst = foldr insert EmptyTree lst

-- Построение списка пар из дерева
listFromTree :: TreeMap v -> [(Integer, v)]
listFromTree EmptyTree = []
listFromTree (Node key val left right) = (listFromTree left) ++ [(key, val)] ++ (listFromTree right)

-- Поиск k-той порядковой статистики дерева 
lenT :: TreeMap v -> Integer
lenT EmptyTree = 0
lenT (Node _ _ left right) = 1 + lenT left + lenT right

kMean :: Integer -> TreeMap v -> (Integer, v)
kMean _ EmptyTree = error "Empty"
kMean i (Node key val left right) 
    | i == lenL = (key, val)
    | i < lenL = kMean i left
    | otherwise = kMean (i - lenL - 1) right
    where lenL = lenT left

abs' :: Integer -> Integer
abs' n | n >= 0    = n
      | otherwise = -n
