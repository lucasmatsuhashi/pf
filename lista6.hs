--1

paridade :: [Int] -> [Bool]
paridade x = map (even) x

--2

prefixos :: [[Char]] -> [[Char]]
prefixos x = map (take 3) x


--3

saudacao :: [[Char]] -> [[Char]]
saudacao x = map ("Oi " ++) x

--4

propriedade :: a -> Bool

filtrar :: (a -> Bool) -> [a] -> [a]
filtrar propriedade [] = []
filtrar propriedade (x:xs) | propriedade x = x : filtrar propriedade xs
                           | otherwise = filtrar propriedade xs

filtrar_compreensao :: (a -> Bool) -> [a] -> [a]
filtrar_compreensao x = [y | y <- x, propriedade y]

--5

pares :: [Int] -> [Int]
pares x = filter even x

--6

solucoes :: (Ord a, Num a) => [a] -> [a]
solucoes x = filter (\y -> ((5*y + 6) < (y*y))) x

--7

maior :: [Int] -> Int
maior x = foldr1 max x

--8

menor_min10 :: [Int] -> Int
menor_min10 x = foldr min 10 (filter (\y -> y > 9) x)

--9

junta_silabas_plural :: [[Char]] -> [Char]
junta_silabas_plural x = foldr (++) "s" x

--10

--10

lst1 = [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20]
lst2 = [20,19,18,17,16,15,14,13,12,11,10,9,8,7,6,5,4,3,2,1]
lst3 = [11,12,13,14,15,16,17,18,19,20,1,2,3,4,5,6,7,8,9,10]
lst4 = [10,9,8,7,6,5,4,3,2,1,20,19,18,17,16,15,14,13,12,11]
lst5 = [11,12,13,14,15,5,4,3,2,1,16,17,18,19,20,10,9,8,7,6]
lst6 = [1,12,3,14,5,15,4,13,2,11,6,17,8,19,20,10,9,18,7,16]
lst7 = [1..1000]
lst8 = [1000,999..1]
lst9 = lst1++[0]
lst10 = [0]++lst3
lst11 = lst1++[0]++lst3
lst12 = lst3++[0]++lst1

lista = [lst1,lst2,lst3,lst4,lst5,lst6,lst7,lst8,lst9,lst10,lst11,lst12]

-- BubbleSort

troca ::(Ord a)=>[a]->[a]
troca [x] = [x]
troca (x:y:z) | x>y = y:troca (x:z)
              |otherwise = x:troca(y:z)

bubbleSort :: (Ord a)=>[a]->[a]
bubbleSort x = bubbleSort2 x (length x)

bubbleSort2 :: (Ord a)=>[a]->Int->[a]
bubbleSort2 x 0 = x
bubbleSort2 x n = bubbleSort2 (troca x) (n-1)

-- SelectionSort

selectionSort :: (Ord a) => [a] -> [a]
selectionSort [] = []
selectionSort x = y : selectionSort (remove y x)
 where y = minimo x 

remove :: (Ord a) => a -> [a] -> [a]
remove y [] = []
remove y (x:xs) |y==x = xs
                |otherwise = x : remove y xs

minimo :: (Ord a) => [a] -> a
minimo [] = undefined
minimo [x] = x
minimo (x:xs) | x <= minimo xs = x
              | otherwise = minimo xs

--InsertionSort

insertionSort :: (Ord a) => [a] -> [a]
insertionSort x = foldr insertionSort2 [] x

insertionSort2 :: (Ord a) => a -> [a] -> [a]
insertionSort2 y [] = [y]
insertionSort2 y (x:xs) | y <= x = y : x : xs
                        | otherwise = x : insertionSort2 y xs

-- QuickSort

quickSort :: (Ord a)=>[a]->[a] 
quickSort [] = []
quickSort (x:y) = quickSort [a | a<-y,a<x] ++ [x] ++ quickSort[b | b<-y,b>x]





main :: IO()
main = return ()