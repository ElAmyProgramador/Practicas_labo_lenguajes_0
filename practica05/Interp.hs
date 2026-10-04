module Interp where

import Grammars
import FuncAux -- funciones dian para hacer ell código más legible
import Tipos -- se movieron acá por el bien de la trama

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
curryApp e (x:xs)   = Just $ foldl App a xs -- e es la expresión :'v
    where
        a = App e x

binaryOp :: (ASA -> ASA -> ASA) -> [ASA] -> Maybe ASA
binaryOp _ []       = Nothing
binaryOp _ [x]      = Nothing
binaryOp f (x:xs)   = Just $ foldl f x xs

-- Desazucara las clausulas ordinarias de cond en If anidados. La alternativa
-- else es el ultimo argumento y se conserva como la rama final.
desugarCond :: [(SASA, SASA)] -> SASA -> Maybe ASA
-- haciendo pattern matching desde la pagina de lesli
desugarCond [] expresion  = desugar expresion
desugarCond ((condicion, expresion0) : cola) expresion1 =
    If <$> desugar condicion <*> desugar expresion0 <*> desugarCond cola expresion1
    -- Al If se le pasa una variable que es la aplicacion recursiva de las tuplas y el resto del cuerpo, por lo que el manejo de funtores devuelve Nothing si uno es Nothing
-- Elimina toda la sintaxis superficial. CondS se traduce a If anidados.
-- LetRecS f definicion cuerpo se traduce usando el identificador Y:
--
--   LetS f (AppS (IdS "Y") (FunS [f] definicion)) cuerpo
--

-- y despues se elimina tambien ese LetS. LetRecS no pertenece al nucleo.
desugar :: SASA -> Maybe ASA
-- un copiado y pegado de la practica anterior
-- Expresiones atómicas
desugar (IdS x)                 = Just $ Id x
desugar (NumS y)                = Just $ Num y
desugar (BooleanS z)            = Just $ Boolean z

-- rehaciendo lo anterior para no tener do
-- Funiciones
desugar (FunS e cuerpo)         = dian1 (curryFun e) (desugar cuerpo) 
-- Supongamos que e es una funcion currificada, primero desazucaramos el cuerpo para obtener la lista que se pera de curryFun y aplicamos el ASA

-- aplicaciones con curry
desugar (AppS s p0)             = dian2 curryApp (desugar s) (mapM desugar p0)

-- las de las binOp (creo se lee mejor que binaryOp), y supongo que podria abstraerse a algo más general
desugar (AddS p)                = dian1 (binaryOp Add) (mapM desugar p)
desugar (SubS p)                = dian1 (binaryOp Sub) (mapM desugar p)

-- La aplicación de mapM_ :: (Foldable t, Monad m) => (a -> m b) -> t a -> m () se usa para aplicar en forma de functor o aplicative hacia listas en lugar de usar <$> en un applicative, dependiendo del prelude este puede ser o no de tipo Mafbe Monad (que por transitividad un functor es applicative y applicative es un paso menos abstracto para una monad) ref: https://hoogle.haskell.org/?q=mapM_
-- volviendo a usarla para mayor libertad y abstracción a la hora de manejar estructuras

-- nuestr NOT
desugar (NotS e)                        = dian1 (\x -> Just $ Not x) (desugar e) -- Not usa una lambda, entonces debemos ponerle la x que viene de desugar e, aunque en teoria creo esto ni siquiera tener un tipo Maybe pero weno :v

-- el condicional
desugar (IfS condicion ent algoMas)     =
    If <$> desugar condicion <*> desugar ent <*> desugar algoMas
-- incomodamente similar a lo de arriba

-- let normal (nuestro Let)
desugar (LetS lambda x y)               = dian2 alpha beta gamma
    where
        alpha   = \a b -> Just (App (Fun lambda b) a)
        beta    = desugar x
        gamma   = desugar y
    -- Esto requiere Justo ya que con las funciones anteriores ya es de tipo Mayxbe, a excepcion de esto
    -- tal vez seria mejor usar mapM

-- let shiny
desugar (LetStarS [] cuerpo)                    = desugar cuerpo -- me parece que esto solo regresa la variable que halla
desugar (LetStarS ((x, s):bindings) cuerpo)     = desugar (LetS x s (LetStarS bindings cuerpo)) -- se hace recursión sobre una lista con elemento(s) y se pasa la lógica de LetS hacia todo su cuerpo, creo esto tambien debería ser applicative en su defecto monádico con mapM
-- a este si casi no le entendí :'v

-- el desugar de CondS que viene en el Grammars.y
desugar (CondS condiciones expresion)   = desugarCond condiciones expresion

-- el LetRecS
desugar (LetRecS f e c) = desugar $ LetS f a c
    where a = AppS (IdS "Y") [FunS [f] e] -- esto ya estaba con nuestro desugar pasado

-- RETO 4: evaluacion perezosa con alcance estatico ------------------------

-- Busca la asociacion mas reciente sin exigir su contenido.
lookupEnv :: Nombre -> Env -> Maybe Value
lookupEnv _ [] = Nothing            
lookupEnv x ((k, v):xs)             
    | x == k    = Just v            
    | otherwise = lookupEnv x xs

-- Exige una cerradura de expresion usando el ambiente guardado. Si al
-- evaluarla se obtiene otra ExprV, continua hasta producir otro valor.
strict :: Value -> Maybe Value
strict (NumV x) = Just (NumV x)
strict (BooleanV b) = Just (BooleanV b)
strict (ClosureV x a env) = Just (ClosureV x a env)
strict (ExprV a env)
    |Just e' <- bigStep env a = strict e'
    |otherwise = Nothing

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
bigStep _ (Num n) = Just (NumV n)
bigStep _ (Boolean b) = Just (BooleanV b)
--operaciones aritméticas
bigStep env (Add x y) = 
    let
        Just e1 = bigStep env x
        Just e2 = bigStep env y

        Just (NumV n) = strict e1           
        Just (NumV m) = strict e2
    in Just(NumV (n + m))

bigStep env (Sub x y)= 
    let
        Just e1 = bigStep env x
        Just e2 = bigStep env y

        Just (NumV n) = strict e1           
        Just (NumV m) = strict e2
    in Just(NumV (n + m))
--Fun
bigStep env (Fun p b) = Just (ClosureV p b env)
--App
bigStep env (App f a) =
    let
        Just (ClosureV p b envFun)= bigStep env f
        envNuevo =[(p, (ExprV a env))] ++ envFun
    in bigStep envNuevo b
--If
bigStep env (If a1 a2 a3) =
    let
        Just w = bigStep env a1
        Just(NumV n) = strict w
    in
        if n == 0 then bigStep env a2
        else bigStep env a3
--Not
bigStep env (Not e) =
    let 
        Just w = bigStep env e
        Just (BooleanV b) = strict w
    in Just (BooleanV (not b))
