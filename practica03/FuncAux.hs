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

