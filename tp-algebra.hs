-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
----------------------- *** TP *** ----------------------------------------------------
-------------------------------------------------------------------------------
-------------------------------------------------------------------------------

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