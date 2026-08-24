module Laboratorio01 where

-- 1
distanciaOrigen :: Double -> Double -> Double
distanciaOrigen a b = sqrt $ a^2 + b^2

-- 2
sumaCuadradosPares :: [Int] -> Int
sumaCuadradosPares lista = sum $ map (^2) (filter even lista)

-- 3
aplicaTresVeces :: (a -> a) -> a -> a
aplicaTresVeces f x = f $ f (f x)

-- 4
varianza2 :: Double -> Double -> Double
varianza2 x y = ((x - m)^2 + (y - m)^2) / 2
    where m = (x + y) / 2

-- 5
clasificaTemperatura :: Int -> String
clasificaTemperatura n
    | n < 1 = "frio extremo"
    | n <= 15 = "frio"
    | n <= 25 = "templado"
    | n <= 35 = "calido"
    | otherwise = "calor extremo"

-- 6
intercala :: a -> [a] -> [a]
intercala _ [] = []
intercala _ [a] = [a]
intercala y (x:xs) = [x] ++ [y] ++ intercala y xs

-- 7
data Expr
    = Lit Int
    | Suma Expr Expr
    | Producto Expr Expr
    deriving (Eq, Show)

evalua :: Expr -> Int
evalua (Lit a) = a
evalua (Suma a b) = evalua a + evalua b
evalua (Producto a b) = evalua a * evalua b
-- Como tal Num tiene estancia Show y Eq, pero asi se puede ver cada Literal como elemento

