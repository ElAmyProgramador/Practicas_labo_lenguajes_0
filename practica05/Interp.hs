module Interp where

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

-- RETO 3: desazucarado ----------------------------------------------------

-- Recupera estas funciones del laboratorio 4. Las funciones y aplicaciones
-- del nucleo siguen siendo unarias, y las operaciones siguen siendo binarias.

-- el copia y pega
curryFun :: [Nombre] -> ASA -> Maybe ASA
curryFun [] _       = Nothing
curryFun [x] e      = Just $ Fun x e
curryFun (x:xs) e
    | elem x xs     = Nothing
    | otherwise     = Fun x <$> curryFun xs e
    -- usando applicative

curryApp :: ASA -> [ASA] -> Maybe ASA
curryApp _ []       = Nothing
curryApp e (x:xs)   = Just $ feldl f x xs

binaryOp :: (ASA -> ASA -> ASA) -> [ASA] -> Maybe ASA
binaryOp _ []       = Nothing
binaryOp _ [x]      = Nothing
binaryOp f (x:xs)   = Just $ foldl f x xs

-- Desazucara las clausulas ordinarias de cond en If anidados. La alternativa
-- else es el ultimo argumento y se conserva como la rama final.
desugarCond :: [(SASA, SASA)] -> SASA -> Maybe ASA
 -- haciendo pattern matching desde la pagina de lesli

-- Elimina toda la sintaxis superficial. CondS se traduce a If anidados.
-- LetRecS f definicion cuerpo se traduce usando el identificador Y:
--
--   LetS f (AppS (IdS "Y") (FunS [f] definicion)) cuerpo
--
-- y despues se elimina tambien ese LetS. LetRecS no pertenece al nucleo.
desugar :: SASA -> Maybe ASA

-- RETO 4: evaluacion perezosa con alcance estatico ------------------------

-- Busca la asociacion mas reciente sin exigir su contenido.
lookupEnv :: Nombre -> Env -> Maybe Value

-- Exige una cerradura de expresion usando el ambiente guardado. Si al
-- evaluarla se obtiene otra ExprV, continua hasta producir otro valor.
strict :: Value -> Maybe Value

-- Semantica de paso grande con alcance estatico y evaluacion perezosa.
--
-- * Id devuelve directamente la asociacion encontrada.
-- * Fun produce ClosureV con el ambiente de definicion.
-- * App exige la posicion de funcion, pero liga el argumento como
--   ExprV argumento ambienteDeLaLlamada.
-- * Add, Sub y Not exigen sus operandos.
-- * If exige solamente la condicion y evalua una sola rama.
--
-- La resta sobre naturales permanece truncada en cero.
bigStep :: Env -> ASA -> Maybe Value
