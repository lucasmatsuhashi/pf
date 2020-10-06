--1

--a



--b

insertionSort :: (Ord a) => [a] -> [a]
insertionSort x = foldr insertionSort2 [] x

insertionSort2 :: (Ord a) => a -> [a] -> [a]
insertionSort2 y [] = [y]
insertionSort2 y (x:xs) | y <= x = y : x : xs
                        | otherwise = x : insertionSort2 y xs

--c

quickSortFilter :: (Ord a) => [a] -> [a]
quickSortFilter [] = []
quickSortFilter (x:xs) = quickSortFilter (filter (\y -> y < x) xs) ++ [x] ++ quickSortFilter (filter (\z -> z >= x) xs) 

--2

--Variacao 1

bolha (x:y:ys) | x > y = (y : fst (bolha (x:ys)), True)
               | otherwise = (x : fst k, snd k)
 where k = bolha (y:ys)
bolha x = (x, False)

bubbleSortv1 x | snd k = bubbleSortv1 (fst k)
               | otherwise = fst k
 where k = bolha x

 --Variacao 2



--3

--Variacao 1

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

--4

--Variacao 1

divide :: (Ord a) => [a] -> [a] -> [a] -> a -> ([a],[a])
divide a b [] y = (a,b)
divide a b (x:xs) y |x<y = divide (x:a) b xs y
                    |otherwise = divide a (x:b) xs y

quickSortV1 :: (Ord a) => [a] -> [a]
quickSortV1 [] = []
quickSortV1 (x:xs) = quickSortV1 (fst (k)) ++ [x] ++ quickSortV1 (snd (k))
 where k = divide [] [] xs x

--Variacao 2 (nao pronto)

--mediana :: [a] -> a

mediana :: (Ord a) => [a] -> a
mediana (x:y:z:zs) |(x>y && y>z) || (z>y && y>x) = y
                |(y>z && z>x) || (x>z && z>y) = z
                |otherwise = x

list_to_element :: (Ord a) => [a] -> a
list_to_element [x] = x

pivot :: (Ord a) => [a] -> a
pivot x = if ((length x) < 3) then list_to_element (take 1 x) else mediana x

list_and_pivot :: (Ord a) => [a] -> ([a],a)
list_and_pivot x = (remove y x, y)
 where y = pivot x

quickSortV2 :: (Ord a) => [a] -> [a]
quickSortV2 [] = []
quickSortV2 x = quickSortV2 (fst (k)) ++ take 1 x ++ quickSortV2 (snd (k))
 where k = divide [] [] snd (list_and_pivot (take 3 x)) fst (list_and_pivot (take 3 x))

--5

mergeSort :: (Ord a) => [a] -> [a]
mergeSort []  = []
mergeSort [x] = [x]
mergeSort xs  = merge (mergeSort ys) (mergeSort zs)
  where
  (ys,zs)     = (take (div (length xs) 2) xs, drop (div (length xs) 2) xs)

merge :: Ord a => [a] -> [a] -> [a]
merge xs [] = xs
merge [] ys = ys
merge (x:xs) (y:ys) | x <= y    = x:merge xs (y:ys)
                    | otherwise = y:merge (x:xs) ys

--6



--a

data Exp a = Val a -- um numero
 | Add (Exp a) (Exp a) -- soma de duas expressoes
 | Sub (Exp a) (Exp a) -- subtração
 | Mult (Exp a) (Exp a)
 | Pot (Exp a) (Exp a)

avalia :: (Num a, Integral a) => Exp a -> a
avalia (Val x) = x
avalia (Add exp1 exp2) = (avalia exp1) + (avalia exp2)
avalia (Sub exp1 exp2) = (avalia exp1) - (avalia exp2)
avalia (Mult exp1 exp2) = (avalia exp1) * (avalia exp2)
avalia (Pot exp1 exp2) = (avalia exp1) ^ (avalia exp1)

--b

--(3+12)*(15-5)^(1*3) e - ((6+8-5+1)*(2+6^2)) 
avalia (Neg exp1) = - (avalia exp1)

expressao1 = avalia (Mult (Add (Val 3) (Val 12)) (Pot (Sub (Val 15) (Val 5)) (Mult (Val 1) (Val 3))))
expressao2 = avalia (Mult (Add (Add (Add (Val 6) (Val 8)) (Neg (Val 5))) (Val 1)) (Add (Val 2) (Pot (Val 6) (Val 2))))

--7

data Hora = AM Int Int
 | PM Int Int deriving (Eq, Ord)

--a

horasDecorridas :: Hora -> Int
horasDecorridas (AM x _) = x
horasDecorridas (PM x _) = x + 12

minutosDecorridos :: Hora -> Int
minutosDecorridos (AM x y) = x * 60 + y
minutosDecorridos (PM x y) = (x + 12) * 60 + y

segundosDecorridos :: Hora -> Int
segundosDecorridos (AM x y) = (x * 60 + y) * 60
segundosDecorridos (PM x y) = ((x + 12) * 60 + y) * 60

--b



--c

--Está feito (deriving (Eq))

--8



--9

data ArvBinInt = Nulo
 |No Int ArvBinInt ArvBinInt deriving Show

emOrdem :: ArvBinInt -> [Int]
emOrdem Nulo = []
emOrdem (No x esq dir) = (emOrdem esq) ++ [x] ++ (emOrdem dir)

arvEx = (No 2 (No 7( No 2 Nulo Nulo) (No 6 (No 5 Nulo Nulo) (No 11 Nulo Nulo))) (No 5 Nulo (No 9 (No 4 Nulo Nulo) Nulo)))

--a

noInterno :: ArvBinInt -> [Int]
noInterno Nulo = []
noInterno (No _ Nulo Nulo) = []
noInterno (No x esq dir) = (noInterno esq) ++ [x] ++ (noInterno dir)

--b

somaNos :: ArvBinInt -> Int
somaNos Nulo = 0
somaNos (No x esq dir) = x + somaNos esq + somaNos dir

--c

pertenceArvore :: ArvBinInt -> Int -> Bool
pertenceArvore Nulo _ = False
pertenceArvore (No x esq dir) y | x == y = True
                                | otherwise = pertenceArvore esq y || pertenceArvore dir y 

main :: IO ()
main = return ()