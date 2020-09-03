-- Nome: Lucas Yudi Matsuhashi
-- Matricula: 11711BCC031

--1

--a

(||) :: Bool->Bool->Bool
True || True = True
False || True = True
False || False = False

--b

(||) :: Bool->Bool->Bool
(||) x y = if x==True or y == True then True else False

--2

type Ponto = (Double, Double)

distancia :: Ponto -> Ponto -> Double
distancia (x1, y1) (x2, y2) = sqrt((x1-x2)**2+(y1-y2)**2)

--3

-- 1:[2,3,4]

  -- [1,2,3,4]

-- 'a':['b','c','d']

  -- "abcd"

-- head [1,2,3]

  -- 1

-- tail [1,2,3]

  -- [2,3]

-- [1,5,2,3]!!1

  -- 5

-- [1,5,2,3]!!3

  -- 3

-- elem 2 [1,5,2,3]

  -- True

-- take 2 [1,5,2,3,7]

  -- [1,5]

-- drop 2 [1,5,2,3,7]

  -- [2,3,7]

-- [1.2] ++ [3.4]

  -- [1,2,3,4]

-- [1..10]

  -- [1,2,3,4,5,6,7,8,9,10]

-- [7,6..3]

  -- [7,6,5,4,3]

-- ['b'..'g']

  -- "bcdefg"

-- take 5 [1,3,..]

  -- [1,3,5,7,9]

-- sum [1..10]

  -- 55

-- maximum [1,5,2,3,7]

  -- 7

-- minimum [1,5,2,3,7]

  -- 1

--4

fatorialGuardas :: Int -> Int
fatorialGuardas x |(x==0) || (x==1) = 1
                  |otherwise = x*fatorialGuardas (x-1)

fatorialCP :: Int -> Int
fatorialCP 1 = 1
fatorialCP 0 = 0
fatorialCP x = x*fatorialCP (x-1)

--5

fibo :: Int -> Int
fibo 0 = 0
fibo 1 = 1
fibo x = fibo (x-1) + fibo (x-2)

--6

n_tri :: Int -> Int
n_tri 1 = 1
n_tri x = x+n_tri (x-1)

--7

type ParFib = (Int,Int)

fibo2 :: Int->Int
fibo2 x = fibo2aux x (1,1)

fibo2aux :: Int -> ParFib -> Int
fibo2aux 1 (x,_) = x
fibo2aux n (x,y) = fibo2aux (n-1) (y,x+y)

--8

potencia2 :: Int->Int
potencia2 0 = 1
potencia2 n = 2*potencia2 (n-1)

--9

--a

prodIntervalo :: Int->Int->Int
prodIntervalo x y |x>=y = 0
                  |otherwise = product [x..y]

--b

fatorial :: Int->Int
fatorial x = prodIntervalo 1 x

--11

--divisao :: Int->Int->(Int->Int)->(Int->Int)
--divisao x y (_,q) |x>=y = divisao (x-y) y (_,q+1)
--                  |otherwise = (y-x,q)

resto_div :: Int->Int->Int
resto_div x y |x>=y = resto_div (x-y) y
              |otherwise = x

div_inteira :: Int->Int->Int
div_inteira x y = div_inteira_aux x y 0

div_inteira_aux :: Int->Int->Int->Int
div_inteira_aux x y q |x<y = q
                      |otherwise = div_inteira_aux (x-y) y (q+1)

--12

mdc_guardas :: Int->Int->Int
mdc_guardas x y |y==0 = x
                |x>y = mdc_guardas y (resto_div x y)
                |otherwise = mdc_guardas y (resto_div y x)

mdc_CP :: Int->Int->Int
mdc_CP x 0 = x
mdc_CP x y = mdc_CP y (resto_div x y)

--13

binomial_guardas :: Int->Int->Int
binomial_guardas x y |y<x = 0
                     |y==0 || y==x = 1
                     |otherwise = binomial_guardas (x-1) y + binomial_guardas (x-1) (y-1)

binomial_CP :: Int->Int->Int
binomial_CP x 0 = 1
binomial_CP x y = if x==y then 1 else binomial_CP (x-1) y + binomial_CP (x-1) (y-1)

--14

--a [5,4..1]
--b ['a','c'..'e']
--c [1,4..16]
--d zip [1,(-2)..(-11)] [1,5..17]

--15

--a

f15_a :: Int->Int->[Int]
f15_a x y = [x..y]

--b

f15_b :: Int->Int->[Int]
f15_b x y = [a | a<-[x..y], even a]
