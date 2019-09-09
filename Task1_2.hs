module Task1_2 where

import Todo(todo)

import Prelude hiding (gcd)

-- синус числа (формула Тейлора)
sin :: Double -> Double
sin x = todo

-- косинус числа (формула Тейлора)
cos :: Double -> Double
cos x = todo

-- наибольший общий делитель двух чисел
gcd :: Integer -> Integer -> Integer
gcd x y = if (x == 0) then y
          else if (y == 0) then x
               else gcd y (mod x y)

-- существует ли полный целочисленный квадрат в диапазоне [from, to)?
doesSquareBetweenExist :: Integer -> Integer -> Bool
doesSquareBetweenExist from to = bnd (to - 1) >= bnd1 from
    where
    bnd = floor . sqrt . fromIntegral;
    bnd1 = ceiling . sqrt . fromIntegral 


-- является ли дата корректной с учётом количества дней в месяце и
-- вискокосных годов?
isDateCorrect :: Integer -> Integer -> Integer -> Bool
isDateCorrect day month year = if (day > 0) && (month > 0) && (year > 0) then 
                                    case month of
                                        2 -> if isLeapYear year then 
                                                day <= 29 else day <= 28
                                        x | elem x [1, 3, 5, 7, 8, 10, 12] -> day < 31
                                        x | elem x [4, 6, 9, 11] -> day < 30
                               else False

isLeapYear :: Integer -> Bool
isLeapYear year = if (year `mod` 4 == 0) && ((year `mod` 100 /= 0) || (year `mod` 400 == 0)) then True
        else False 

-- возведение числа в степень, duh
-- готовые функции и плавающую арифметику использовать нельзя
pow :: Integer -> Integer -> Integer
pow x 0 = 1
pow x 1 = x
pow x y = if even y then x * pow x (y - 1)
                    else x * (pow (x * x) ((y-1) `div` 2))


-- является ли данное число простым?
isPrime :: Integer -> Bool
isPrime x = if x <= 1 then error "wrong number"
                      else [y | y <- [1..x], x `mod` y == 0] == [1,x]

