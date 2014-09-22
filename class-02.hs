-- 1.1
-- Написать функцию, которая разбивает промежуток времени в секундах на часы, минуты и секунды.
-- Результат возвращать в виде кортежа из трёх элементов. Реализовать также обратное преобразование.
sec2hms :: Int -> (Int, Int, Int)
sec2hms s = (s `div` 360, (s `mod` 360) `div` 60, s `mod` 60)

{-
> sec2hms 461
(1,1,1)
-}

hms2sec :: (Int, Int, Int) -> Int
hms2sec (h, m, s) = h*360 + m*60 + s

{-
> hms2sec (1,1,1)
421
-}

-- Реализовать с помощью hms2sec (здесь параметры заданы по отдельности)
hms2sec' :: Int -> Int -> Int -> Int
hms2sec' h m s = hms2sec (h,m,s)

{-
> hms2sec' 2 1 1
781
-}

-- должно быть True
test1 = and $ map (\x -> x == hms2sec (sec2hms x)) [1,10..10000]

{-
> test1
True
-}

-- 1.2
-- Написать функции, вычисляющие
-- а) длину отрезка по координатам его концов;
-- б) периметр и площадь треугольника по координатам вершин.
type Point = (Double, Double)
distance :: Point -> Point -> Double
distance (x1, y1) (x2, y2) = sqrt((x2-x1)^2 + (y2-y1)^2)

{-
> distance (1, 2) (4, 6)
5.0
-}

triangle :: Point -> Point -> Point -> (Double, Double)

triangle (x1, y1) (x2, y2) (x3, y3) = (p, s)
  where
  d1 = distance (x1, y1) (x2, y2)
  d2 = distance (x2, y2) (x3, y3)
  d3 = distance (x3, y3) (x1, y1) 
  p = d1 + d2 + d3
  p' = p/2
  s = sqrt(p'*(p' - d1)*(p' - d2)*(p' - d3))

-- Во всех следующих заданиях использование стандартных функций обработки списков не допускается.
-- Все решения должны реализовываться рекурсивными функциями.
-- 2.1
-- Определить рекурсивную функцию, определяющую количество чётных элементов списка
nEven :: Integral a => [a] -> Int
nEven [] = 0
nEven (x:xs) = if x `mod` 2 == 0 then 1 + nEven xs else nEven xs

-- 2.2
-- Увеличить все элементы заданного списка в два раза.
-- Указание: в решении может понадобиться операция конструирования списка:
-- > 1 : [2,3,4]
-- [1,2,3,4]
doubleElems :: Num a => [a] -> [a]
doubleElems [] = []
doubleElems (x:xs)= x*2 : doubleElems xs

-- 2.3
-- Дан список целых чисел. Сформировать новый список, содержащий только нечетные элементы исходного.
fltOdd :: Integral a => [a] -> [a]
fltOdd [] = []
fltOdd (x:xs) = if x `mod` 2 /= 0 then x : fltOdd xs else fltOdd xs
-- 2.4
-- Написать следующие функции обработки списков:
-- а) удалить все отрицательные элементы;
-- б) увеличить элементы с чётными значениями в два раза;
-- в) переставить местами чётные и нечётные по порядку следования элементы
-- (для списков нечётной длины отбрасывать последний элемент).

delNeg :: (Num a, Ord a) => [a] -> [a]
delNeg [] = []
delNeg (x:xs) = if x < 0 then delNeg xs else x : delNeg xs

doublePos :: Integral a => [a] -> [a]
doublePos [] = []
doublePos (x:xs) = if x `mod` 2 == 0 then x*2 : doublePos xs else x : doublePos xs

changePos :: [a] -> [a]
changePos [] = []
changePos [x] = []
changePos (x:y:xs) = y : x : changePos xs

-- 2.5
-- Даны два списка целых чисел. Сформировать список, каждый элемент которого равен сумме
-- соответствующих элементов исходных списков. Предусмотреть ситуацию списков разной длины.

combine_plus :: [Integer] -> [Integer] -> [Integer]
combine_plus [] ys = ys
combine_plus xs [] = xs
combine_plus (x:xs) (y:ys) = x+y : combine_plus xs ys

-- 2.6
-- Даны два списка. Сформировать новый список, содержащий пары из соответствующих элементов
-- исходных списков. Хвост более длинного списка отбросить.

listOfPair :: [a] -> [b] -> [(a,b)]
listOfPair [] ys = []
listOfPair xs [] = []
listOfPair (x:xs) (y:ys) = (x, y) : listOfPair xs ys

-- 2.7
-- Написать функции, которые по заданному n возвращают список, состоящий из n первых натуральных чисел
-- а) в порядке убывания;
-- б) в порядке возрастания.

returnDecrList :: Integer -> [Integer]
returnDecrList 0 = []
returnDecrList n = n : returnDecrList(n-1)

returnIncrList :: Integer -> [Integer]
returnIncrList 0 = []
returnIncrList n = returnIncrList (n-1) ++[n]

-- 2.8
-- Дан элемент типа a и список [a]. Вставить между всеми элементами списка заданный элемент.
insertElem :: a -> [a] -> [a]
insertElem a [] = []
insertElem a [x] = [x]
insertElem a (x:y:xs) = x : a : insertElem a (y:xs)

-- 2.9
-- Написать функцию, которая разбивает список на два подсписка: элементы из начала списка,
-- совпадающие с первым элементом, и все остальные элементы, например:
-- [1,1,1,2,3,1] -> ([1,1,1], [2,3,1]).

distrList :: Eq a => [a] -> ([a], [a])
distrList [] = ([],[])
distrList (x:xs) = distrList' xs [x]
  where 
    distrList':: Eq a => [a] -> [a] -> ([a],[a])
    distrList' [] ys = (ys, [])
    sdistrList' (x:xs) (y:ys) = if (x == y) then (distrList' xs (x: (y:ys))) else ((y:ys),(x:xs))

--3
-- Даны типовые аннотации функций. Попытайтесь догадаться, что они делают, и напишите их
-- рекурсивные реализации (если вы можете предложить несколько вариантов, реализуйте все):
-- а) [a] -> Int -> a
--
returnElem :: [a] -> Int -> a
returnElem [] n = error "Element is not found!"
returnElem (x:xs) n 
    | n < 1 = error "Index start from 1"
    | n == 1 = x 
    | otherwise = returnElem xs (n-1)

-- б) Eq a => [a] -> a -> Bool
findElem :: Eq a => [a] -> a -> Bool
findElem [] n = False
findElem (x:xs) n 
    | x == n = True
    | otherwise = findElem xs n

-- в) [a] -> Int -> [a]
deleteElem :: [a] -> Int -> [a]
deleteElem [] 0 = []
deleteElem (x:xs) n 
    | n < 0 = error "Count of elements can't be negative!" 
	| n == 1 = xs
    | n >= length(xs) = []
	| otherwise = deleteElem xs (n-1)

-- г) a -> Int -> [a]
replicateList :: a -> Int -> [a]
replicateList _ 0 = []
replicateList a n 
    | n == 1 = [a]
    | otherwise = a : replicateList a (n-1)
	
-- д) [a] -> [a] -> [a]
insertList :: [a] -> [a] -> [a]
insertList [] ys = ys
insertList xs [] = xs
insertList [x] [y] = [x,y]
insertList (x:xs) (y:ys) = x : y : insertList xs ys 

-- е) Eq a => [a] -> [[a]]
groupList :: Eq a => [a] -> [[a]]
groupList [] = [[]]
groupList [x] = [[x]]
groupList (x:y:xs) = if x==y then [x,y]: groupList xs else [x] : groupList(y:xs)

-- ж) [a] -> [(Int, a)]

pairOfIndAndElem :: [a] -> [(Int, a)]
pairOfIndAndElem [] = []
pairOfIndAndElem xs = pairOfIndAndElem' xs 1
    where 
    pairOfIndAndElem' :: [a] -> Int -> [(Int, a)]
    pairOfIndAndElem' [] n = []
    pairOfIndAndElem' (x:xs) n = (n,x) : pairOfIndAndElem' xs (n+1)
	

-- з) Eq a => [a] -> [a]
reverseList :: Eq a => [a] -> [a]
reverseList [] = []
reverseList (x:xs) = reverseList xs ++[x] 

