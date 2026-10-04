-- Debido a la abstracción del tipo Maybe y a la especifidad vaga del Maybe se usará monad bind para pasar las funciones a Maybe

module FuncAux where

import Grammars
import Tipos
{-
-- tipos de datos
data ASA
    = Id Nombre
    | Num Int
    | Boolean Bool
    | Add ASA ASA
    | Sub ASA ASA
    | Not ASA
    | Fun Nombre ASA
    | App ASA ASA
    | If ASA ASA ASA
    deriving (Eq, Show)

data Value
    = NumV Int
    | BooleanV Bool
    | ClosureV Nombre ASA Env
    | ExprV ASA Env
    deriving (Eq, Show)

type Env = [(Nombre, Value)]
-}

dian1 :: (a -> Maybe ASA) -> Maybe a -> Maybe ASA
dian1 aplicacion subExpr    = subExpr >>= aplicacion

dian2 :: (a -> b -> Maybe ASA) -> Maybe a -> Maybe b -> Maybe ASA
dian2 aplicacion subExpr0 subExpr1  = subExpr0 >>= \acc0 -> subExpr1 >>= \acc1 -> aplicacion acc0 acc1

-- solución del foro https://discourse.haskell.org/t/what-are-the-internals-of-a-bind-operation-in-a-monad/6325
