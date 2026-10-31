module Laboratorio06 where

-- 1. Potencia --------------------------------------------------------------

-- Precondicion: el exponente es mayor o igual que cero.
potencia :: Int -> Int -> Int
potencia _ 0            = 1
potencia n m =
    n * potencia n (m - 1) -- aqui preferiria que usaramos pred, pero creo nos dijeron que no usaramos nada de lo que está en el Prelude (basicamente cualquier función nativa que no sean (+), (*), (:) de acuerdo a: https://hackage-content.haskell.org/package/base-4.22.0.0/docs/Prelude.html)
 -- pred :: Enum a => a -> a; base Prelude Data.Enum GHC.Enum, ghc GHC.Prelude.Basic, ghc-internal GHC.Internal.Enum, incipit-base Incipit.Bas; ref: https://hoogle.haskell.org/?hoogle=pred

potenciaCola :: Int -> Int -> Int
potenciaCola n m        = potenciaCola' n m 1
    where -- este uso de where es curioso
        potenciaCola' :: Int -> Int -> Int -> Int
        potenciaCola' _ 0 acc = acc -- si no hay nada que multiplicar, entonces se devuelve el acumulador
        potenciaCola' n m acc = acc * potenciaCola' n (m - 1) (acc * n) -- para cuando m = 0, se guarde el valor

potenciaCPS :: Int -> Int -> (Int -> r) -> r
potenciaCPS _ 0 k = k 1 -- suponiendo que sea cierto que para todo a^0 = 1, k captura el valor y devuelve lo que capture k (que supongo por el tipo, r :: Int y así k :: Int -> Int)
potenciaCPS n m k = potenciaCPS n (m - 1) (\res -> k (n * res)) -- aqui llamamos a la funcion recursivamente t como k es una función entonces se vale una lambda, en este caso k captura al resultados de la recursión

-- 2. Longitud de una lista -------------------------------------------------

longitud :: [a] -> Int
longitud []         = 0
longitud (_ : xs)   = 1 + longitud xs

longitudCola :: [a] -> Int
longitudCola lista                  = longitudCola' lista 0
    where
        longitudCola' :: [a] -> Int -> Int
        longitudCola' [] acc        = acc
        longitudCola' (_:xs) acc    = longitudCola' xs (acc + 1) -- para este paso hacemos que el acumulador vaya incrementando

longitudCPS :: [a] -> (Int -> r) -> r
longitudCPS [] k                    = k 0 -- el cero porque directamente el valor de longitud de una lista bacía, aunque tambien serviría longitudCola []
longitudCPS (_:xs) k                = longitudCPS xs (\resumen -> k (1 + resumen)) -- el acumulador se va sumando

-- 3. Reversa de una lista --------------------------------------------------

reversa :: [a] -> [a]
reversa [] = []
reversa (x : xs) = reversa xs : [x] -- no es algo personal, pero prefiero que las listas se concatenan usando el operando (:), porque aveces con listas con clase de tipo a, puede dar errores :(

reversaCola :: [a] -> [a]
reversaCola = undefined

reversaCPS :: [a] -> ([a] -> r) -> r
reversaCPS = undefined

-- 4. Concatenacion de una lista de listas ---------------------------------

concatena :: [[a]] -> [a]
concatena [] = []
concatena (xs:xss) = xs : concatena xss

concatenaCola :: [[a]] -> [a]
concatenaCola = undefined

concatenaCPS :: [[a]] -> ([a] -> r) -> r
concatenaCPS = undefined

-- 5. Intercalado de dos listas --------------------------------------------

intercala :: [a] -> [a] -> [a]
intercala [] ys = ys
intercala xs [] = xs
intercala (x : xs) (y : ys) =
    x : y : intercala xs ys

intercalaCola :: [a] -> [a] -> [a]
intercalaCola = undefined

intercalaCPS :: [a] -> [a] -> ([a] -> r) -> r
intercalaCPS = undefined

