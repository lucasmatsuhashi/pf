--1

triangulo :: Double->Double->Double->String
triangulo x y z |(x+y+z /= 180) || x==0 || y==0 || z==0 = "nao_triangulo"
                |x==y && y==z && x==60 = "equilatero"
                |x==90 || y==90 || z==90 = "retangulo"
                |x>90 || y>90 || z>90 = "obtuso"
                |otherwise = "simples"

--2

equacao :: Double->Double->Double->(Double,Double)
equacao a b c |a==0 = ((-c/b),a)
              |otherwise = ((-b-(sqrt(b**2-4*a*c)))/(2*a),(-b+(sqrt(b**2-4*a*c)))/(2*a))

--3

type Data = (Int,Int,Int)

compara_data :: Data->Data->Double
compara_data (a1,m1,d1) (a2,m2,d2) |(a1,m1,d1) < (a2+2,m2,d2) = 0.15
                                   |(a1,m1,d1) < (a2+11,m2,d2) = 0.4
                                   |(a1,m1,d1) >= (a2+70,m2,d2) = 0.5
                                   |otherwise = 1.0

passagem :: Double->Data->Data->Double
passagem v (d1,m1,a1) (d2,m2,a2) = v * compara_data (a1,m1,d1) (a2,m2,d2)

--4

lista1_15 = [1..15]

--a

gera1 :: [Int]
gera1 = [x^2 | x <- lista1_15, odd x, x >= 4, x<=14]

--b

--gera2 :: [(Int,Int)]
gera2 = [(x,y) | x <- lista1_15, y <- [x..2*x], x>=1, x<=4]

--c

gera3 = [[1..x] | x <- [10..15]]

--d

gera4 = [[(x,x+1)] | x <- lista1_15, odd x]

--e

--gera5 = [x+y | (x,y) <- gera4]
gera5 = [x | (x,y) <- gera4]

--5

--a

contaNegM2 :: [Int]->Int
contaNegM2 xs = length [x | x <- xs, x<0, even x]

--b

listaNegM2 :: [Int]->[Int]
listaNegM2 xs = [x | x <- xs, x<0, even x]

--6

distancia :: [(Float,Float)]->[Float]
distancia xs = [sqrt(x^2+y^2) | (x,y) <- xs]

--7

fatores :: Int->[Int]
fatores x = [y | y <- [1..x`div`2], x `mod` y == 0] ++ [x]

primos :: Int->Int->[Int]
primos x y = [z | z <- [x..y], fatores (z) == [1,z]]

--8

mdc::Int->Int->Int
mdc x y |x < y = mdc y x
        |y == 0 = x
        |otherwise = mdc y (mod x y)

mmc2 :: Int->Int->Int
mmc2 x y = (x*y) `div` (mdc x y)

mmc :: Int->Int->Int->Int
mmc x y z = mmc2 x (mmc2 y z)

--9 (incompleto)



--10

fizzbuzz :: Int->[[Char]]
fizzbuzz n = [if i `mod` 15 == 0 then "FizzBuzz" else if i `mod` 3 == 0 then "Fizz" else if i `mod` 5 == 0 then "Buzz" else "No" | i <- [1..n]]

--11 (incompleto)



--12 

unica_ocorrencia_aux :: Int->[Int]->Bool
unica_ocorrencia_aux y [] = True
unica_ocorrencia_aux y (x:xs) |y==x = False
                              |otherwise = unica_ocorrencia_aux y xs

unica_ocorrencia :: Int->[Int]->Bool
unica_ocorrencia y [] = False
unica_ocorrencia y (x:xs) |y==x = unica_ocorrencia_aux y xs
                          |otherwise = unica_ocorrencia y xs


--13

intercala :: [a] -> [a] -> [a]
intercala [] x = x
intercala x [] = x
intercala (x:xs) (y:ys) = [x,y] ++ intercala xs ys

--14

type Agenda = (String, String, String, String)

recupera_nome :: String -> [Agenda] -> String
recupera_nome x [] = "Email desconhecido"
recupera_nome x ((nome, _, _, email):xs) |x==email = nome
                                         |otherwise = recupera_nome x xs

--15 (incompleto)

type Pessoa = (String, Float, Int, Char)
pessoas :: [Pessoa]
pessoas = [ ("Rosa", 1.66, 26, 'F'),("Joao", 1.85, 26, 'C'),("Maria", 1.55, 62, 'S'),("Jose", 1.78, 42, 'C'),("Paulo", 1.93, 25, 'S'),("Clara", 1.70, 33, 'C'),("Bob", 1.45, 21, 'C'),("Rosana", 1.58, 39, 'S'),("Daniel", 1.74, 72, 'S'),("Jocileide", 1.69, 18, 'S')]

--16

insert :: Ord t => t -> [t] -> [t]
insert x [] = [x] 
insert x (y:ys) = if x <= y
                  then x:y:ys
                  else y : insert x ys

--17

reverte [] = []
reverte (x:xs) = reverte xs ++ [x]

--18

pertence _ [] = False
pertence y (x:xs) |x==y = True
                  |otherwise = pertence y xs

unique [] = []
unique (x:xs) |pertence x xs = unique xs
              |otherwise = x:unique xs

--19

disponiveis = [1,2,5,10,20,50,100]
notasTroco :: Int -> [[Int]]
notasTroco 0 = [[]]
notasTroco valor = [v:vs|v <- disponiveis,valor >= v,vs<-notasTroco(valor-v) ] 

--20 (incompleto)

queens n = solve n
    
    where
    solve 0 = [[]]
    solve (k+1) = [q:b | b <- solve k, q <- [0..(n-1)], safe q b]
    safe q b = and [not (checks q b i) | i <- [0..(length b-1)]]
    checks q b i = q == (b!!i) || abs (q - (b!!i)) == i+1


main :: IO()
main = return()