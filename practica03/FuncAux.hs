-- módulo de funciones auxiliares para no sobrecargar el Interp.hs :)
module FuncAux where

import Grammars

-- Funciones auxiliares (creadas por Estefany):

-- Se recomienda usar en version superior
eliminaElemento :: String -> [String] -> [String]
eliminaElemento y lst = filter (/= y) lst
-- que creo puede generalizarse a eliminaElemento :: Eq a => a -> [a] -> [a]; aunque en la practica solo tengamos [String] -- "Amy"

eliminaEnLista :: [String] -> [String] -> [String]
eliminaEnLista [] y = y
eliminaEnLista (x:xs) y = eliminaEnLista xs (eliminaElemento x y)

-- funcion auxiliar para susMany
sustMAuxiliar :: String -> [Binding] -> ASA
sustMAuxiliar x []  = Id x
sustMAuxiliarx x ((e1, x1):ys)
    | x == e1   = x1 -- nuevamente ña funcion (==) debe tener espacio como nuestro miniLisp :3
    | otherwise = sustMAuxiliar x ys

-- En teoria deberia haber aquí mas funciones, sin embargo algunas necesitaban de la definicion en Interp.hs, por lo que buscaban interpretarlas pero al necesitar de este modulo lo intontaria interpretar, formando un bucle
