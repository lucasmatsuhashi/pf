--1

lst1 = [x*2 | x <- [1..10], x*2 >= 12]

	--[12,14,16,18,20]

lst2 = [ x | x <- [50..100], mod x 7 == 3]

	--[52,59,66,73,80,87,94]

lst3 = [ x | x <- [10..20], x /= 13, x /= 15, x /= 19]

	--[10,11,12,14,16,17,18,20]

lst4=[(x,y)| x <- [1..4], y <- [x..5]]

	--[(1,1),(1,2),(1,3),(1,4),(1,5),(2,2),(2,3),(2,4),(2,5),(3,3),(3,4),(3,5),(4,4),(4,5)]

--2

quadrados :: Int->Int->[Int]
quadrados x y = [i^2 | i <- [x..y]]

--3

seleciona_impares :: [Int]->[Int]
seleciona_impares x = [y | y <- x, odd y]

--4

tabuada :: Int->[Int]
tabuada x = [x*i | i <- [1..10]]

--5

bissexto :: Int -> Bool
bissexto (ano) |(mod ano 400 == 0) = True
               |(mod ano 4 == 0) && (mod ano 100 /= 0) = True
               |otherwise = False

bissextos :: [Int]->[Int]
bissextos x = [y | y <- x, bissexto y]

--6

sublistas :: [[Int]]->[Int]
sublistas x = [y | z <- x, y <- z]

--7

--8

npares :: [Int] -> Int
npares [] = 0
npares (x:xs) |even x = 1 + npares xs
              |otherwise = npares xs

--9

produtorio :: [Int] -> Int
produtorio [] = 0
produtorio [x] = x
produtorio (x:xs) = x*produtorio xs

--10

comprime :: [[a]] -> [a]
comprime [] = []
comprime (x:xs) = x ++ comprime xs

--11

tamanho :: [a] -> Int
tamanho [] = 0
tamanho (x:xs) = 1 + tamanho xs

--12

npertence :: Int->[Int]->Bool
npertence _ [] = True
npertence y (x:xs) |x==y = False
                  |otherwise = npertence y xs

uniaoNRec :: [Int]->[Int]->[Int]
uniaoNRec x y = (++) x [i | i<-y, npertence i x]

--13

uniaoNRec2 :: [Int]->[Int]->[Int]
uniaoNRec2 x [] = x
uniaoNRec2 x (y:ys) |npertence y x = uniaoRec2 x ys ++ [y]
                   |otherwise = uniaoRec2 x ys

uniaoRec2 x y = uniaoNRec2 (x) (reverse y)