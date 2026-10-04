-- Aqui van los tipos para tener todo bien ordenadito
module Tipos where

import Grammars

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
