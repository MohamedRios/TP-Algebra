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
esPrimo n = nohayDivisorPropio n (n - 1)

-- Busca si existe un divisor propio. Es decir, divisor distinto de sí mismo y 1.
nohayDivisorPropio :: Integer -> Integer -> Bool
nohayDivisorPropio _ 1 = True
nohayDivisorPropio n m
    | mod n m /= 0 = nohayDivisorPropio n (m - 1)
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

--Verifica si el primer elemento de una lista de círculos se encuentra repetido
estaRepetidoPrimero :: [Circulo] -> Bool
estaRepetidoPrimero (c:cs) = circuloRepetido c cs

--Agrega un elemento n a cada lista de la lista de listas dada
agregarElem :: Integer -> [[Integer]] -> [[Integer]]
agregarElem n [] = []
agregarElem n (xs:xss) = (n:xs):(agregarElem n xss)

--Dadu un entero n y una lista de enteros, arma una lista agregando n a cada posición posible
--Por ejemplo: formasDeAgregar 3 [1,2] -> [[3,1,2],[1,3,2],[1,2,3]]
formasDeAgregar :: Integer -> [Integer] -> [[Integer]]
formasDeAgregar n [] = [[n]]
formasDeAgregar n (x:xs) = (n:(x:xs)):(agregarElem x (formasDeAgregar n xs))

-- Aplico formasDeAgregar n a cada lista de la lista de listas dada
permAux :: Integer -> [[Integer]] -> [[Integer]]
permAux n [] = []
permAux n (xs:xss) = (formasDeAgregar n xs) ++ (permAux n xss)

permutaciones :: Integer -> [[Integer]]
permutaciones 1 = [[1]]
permutaciones n = permAux n (permutaciones (n-1))


listaCirculosPrimos :: Integer -> [Circulo]
listaCirculosPrimos n = filtrarCirculosPrimos (permutaciones n)

filtrarCirculosPrimos :: [Circulo] -> [Circulo]
filtrarCirculosPrimos [] = []
filtrarCirculosPrimos (xs:xss)
 | esCirculoPrimo xs && (not (estaRepetidoPrimero (xs:xss))) = xs:(filtrarCirculosPrimos xss)
 | otherwise = filtrarCirculosPrimos xss

--Retorna la cantidad de círculos de la lista dada
contarCirculos :: [Circulo] -> Integer
contarCirculos [] = 0
contarCirculos (c:cs) = 1 + (contarCirculos cs)

--Cuenta la cantidad de círculos primos de orden n. Se considera n >= 2
contarCirculosPrimos :: Integer -> Integer
contarCirculosPrimos n = contarCirculos (listaCirculosPrimos n)

