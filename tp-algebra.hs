--Consideramos que al menos tiene 1 elemento (círculo de orden 1) y que dos círculos de orden n tienen los mismos elemento
type Circulo = [Integer]

--Recibe Circulo y un elemento x del mismo y rota hasta que x sea el de más a la izquiera en la representación como lista
rotarCirculoHasta :: Circulo -> Integer -> Circulo
rotarCirculoHasta (x:xs) n 
    | x /= n = rotarCirculoHasta (xs ++ [x]) n
    | otherwise = (x:xs)

-- Considera que son círculos del mismo orden y que empiezan por el mismo elemento
sonCirculosIgualesAlRotar :: Circulo -> Circulo -> Bool
sonCirculosIgualesAlRotar [x] [y] = x == y
sonCirculosIgualesAlRotar (x:xs) (y:ys)
    | x == y = sonCirculosIgualesAlRotar xs ys
    | otherwise = False

sonCirculosIguales :: Circulo -> Circulo -> Bool
sonCirculosIguales (x:xs) (y:ys)
    | length (x:xs) == length (y:ys) = sonCirculosIgualesAlRotar (x:xs) (rotarCirculoHasta (y:ys) x)
    | otherwise = False

--Recibe un entero y verifica si es Primo o no
esPrimo :: Integer -> Bool
esPrimo 1 = False
esPrimo n = esPrimoAux n (n - 1)

--Auxiliar de esPrimo
esPrimoAux :: Integer -> Integer -> Bool
esPrimoAux _ 1 = True
esPrimoAux n m
    | mod n m /= 0 = esPrimoAux n (m - 1)
    | otherwise = False

--Suma los numeros adyecentes de la lista y verifica que sean primos. Se repite hasta que el contador llegue a 0. (Tamaño del Circulo)
sumaDeAdyecentesEsPrimo :: Circulo -> Int -> Bool
sumaDeAdyecentesEsPrimo _ 0 = True
sumaDeAdyecentesEsPrimo (x:y:xs) n
    | esPrimo (x + y) = sumaDeAdyecentesEsPrimo ((y:xs) ++ [x]) (n - 1)
    | otherwise = False

esCirculoPrimo :: Circulo -> Bool
esCirculoPrimo c = sumaDeAdyecentesEsPrimo c (length c)

--Verifica si un Circulo ya se encuenta en una lista de Circulos
circuloRepetido :: Circulo -> [Circulo] -> Bool
circuloRepetido _ [] = False
circuloRepetido c (cl:cls)
    | not (sonCirculosIguales c cl) = circuloRepetido c cls
    | otherwise = True

estaRepetidoPrimero :: [Circulo] -> Bool
estaRepetidoPrimero (c:cs) = circuloRepetido c cs

agregarelem :: Integer -> [Circulo] -> [Circulo]
agregarelem n [] = []
agregarelem n (xs:xss) = (n:xs):(agregarelem n xss)

variaciones :: Integer -> Circulo -> [Circulo]
variaciones n [] = [[n]]
variaciones n (x:xs) = (n:(x:xs)):(agregarelem x (variaciones n xs))

permaux :: Integer -> [Circulo] -> [Circulo]
permaux n [] = []
permaux n (xs:xss) = (variaciones n xs) ++ (permaux n xss)

permutaciones :: Integer -> [Circulo]
permutaciones 1 =[[1]]
permutaciones n = permaux n (permutaciones (n-1))


listaCirculosPrimos :: Integer -> [Circulo]
listaCirculosPrimos n = listaux (permutaciones n)

listaux :: [Circulo] -> [Circulo]
listaux [] = []
listaux (xs:xss)
 | estaRepetidoPrimero (xs:xss) = listaux xss
 | esCirculoPrimo xs = xs:(listaux xss)
 | otherwise = listaux xss

contadorDeLista :: [Circulo] -> Integer
contadorDeLista [] = 0
contadorDeLista (c:cs) = 1 + (contadorDeLista cs)

contarCirculosPrimos :: Integer -> Integer
contarCirculosPrimos n = contadorDeLista (listaCirculosPrimos n)