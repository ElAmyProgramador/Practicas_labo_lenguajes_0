module Interp where

import Grammars
import FuncAux

-- funcion auxiliar freeVarsEnLlista
freeVarsEnLista :: [ASA] -> [String]
freeVarsEnLista []      = []
freeVarsEnLista (x:xs)  = freeVars x ++ freeVarsEnLista xs

-- RETO 3: sustitucion nominal que evita captura -- Estefany

freeVars :: ASA -> [String]
freeVars (Id x)                 = [x]
freeVars (Num n)                = []
freeVars (Boolean b)            = []
freeVars (Not e)                = freeVars(e)
freeVars (And lstASA)           = freeVarsEnLista lstASA
freeVars (Or lstASA)            = freeVarsEnLista lstASA
freeVars (Add lstASA)           = freeVarsEnLista lstASA
freeVars (Sub lstASA)           = freeVarsEnLista lstASA
freeVars (Mul lstASA)           = freeVarsEnLista lstASA
freeVars (Div lstASA)           = freeVarsEnLista lstASA
freeVars (Lt lstASA)            = freeVarsEnLista lstASA
freeVars (Gt lstASA)            = freeVarsEnLista lstASA
freeVars (Le lstASA)            = freeVarsEnLista lstASA
freeVars (Ge lstASA)            = freeVarsEnLista lstASA
freeVars (EqP e1 e2)            = (freeVars e1) ++ (freeVars e2)
freeVars (Expt e1 e2)           = (freeVars e1) ++ (freeVars e2)
freeVars (ZeroP e)              = freeVars e
freeVars (Add1 e)               = freeVars e
freeVars (Sub1 e)               = freeVars e
freeVars (Let bindings body)    =
    let varDeclaradas           = map fst bindings --lista de las x1,...,xn de los pares ordenados (xn, en)
        expresiones             = map snd bindings --lista de expresiones e1,.., en en los pares ordanados (xn, en)
        varLibreEnExpresiones   = freeVarsEnLista expresiones
        varLibreEnBody          = freeVars body
        cuerpoSinVarDeclaradas  = eliminaEnLista varDeclaradas varLibreEnBody
    in varLibreEnExpresiones ++ cuerpoSinVarDeclaradas
freeVars (LetStar [] body)      = freeVars body
freeVars (LetStar ((x1, e1): bindingsRestantes) body) =
    freeVars e1 ++ eliminaElemento x1 (freeVars (LetStar bindingsRestantes body)) -- no $ :(

-- función auxiliar de names y bindings

eEnBindingsAuxiliar ::  [Binding] -> [String]
eEnBindingsAuxiliar [] = []
eEnBindingsAuxiliar ((_, eiesimo): xs) = names eiesimo ++ eEnBindingsAuxiliar xs

namesAuxiliar :: [Binding] -> ASA -> [String]
namesAuxiliar bindings e    =
    let xEnBindings         = map (\(x, _) -> x) bindings
        eEnBindings         = eEnBindingsAuxiliar bindings
        namesBody           = names e
    in xEnBindings ++ eEnBindings ++ namesBody

namesAuxiliar2 :: [ASA] -> [String] -- "Amy"
namesAuxiliar2 lstASA = concatMap names lstASA -- concatMap es un map sobre String, f cama su nombre lo dice, concatena, además de tener instancia de la clase foldable, lo que la hace más predecible

names :: ASA -> [String]
names (Id x)                = []
names (Num n)               = []
names (Boolean b)           = []
names (Not e )              = names e
names (And lstASA)          = namesAuxiliar2 lstASA
names (Or lstASA)           = namesAuxiliar2 lstASA
names (Add lstASA)          = namesAuxiliar2 lstASA
names (Sub lstASA)          = namesAuxiliar2 lstASA
names (Mul lstASA)          = namesAuxiliar2 lstASA
names (Div lstASA)          = namesAuxiliar2 lstASA
names (Gt lstASA)           = namesAuxiliar2 lstASA
names (Lt lstASA)           = namesAuxiliar2 lstASA
names (Le lstASA)           = namesAuxiliar2 lstASA
names (Ge lstASA)           = namesAuxiliar2 lstASA
names (EqP e1 e2)           = names e1 ++ names e2
names (Expt e1 e2)          = names e1 ++ names e2
names (ZeroP e)             = names e
names (Add1 e)              = names e
names (Sub1 e)              = names e
names (Let bindings e)      = (namesAuxiliar bindings e)
names (LetStar bindings e)  = (namesAuxiliar bindings e)

--freshName :: [String] -> String

-- funcion generalizadora del map para listas de ASA (tal vez quedaria bien con curry, pero creo todos los parametros son minimos necesarios)

anyaMap :: ([ASA] -> ASA) -> [ASA] -> String -> ASA -> ASA
anyaMap asaF lstASA x exp =
    asaF $ map bartolo lstASA
    where
        bartolo = \e -> sust e x exp

sust :: ASA -> String -> ASA -> ASA
sust (Id x) y exp
    | x == y    = exp
    | otherwise = Id x
-- creo esto se puede pasar con casos, o sino con gurdas generalizando el contructor a lo que es (una función)

sust (Num n) _ _        = Num n
sust (Boolean b) _ _    = Boolean b
sust (Not e) y exp      = Not $ sust e y exp
sust (Add1 e) y exp     = Add1 $ sust e y exp
sust (Sub1 e) y exp     = Sub1 $ sust e y exp
sust (ZeroP e) y exp    = ZeroP $ sust e y exp
sust (Expt e1 e2) y exp = Expt (sust e1 y exp) (sust e2 y exp)
sust (EqP e1 e2) y exp  = EqP (sust e1 y exp) (sust e2 y exp)

-- usando anyaMap
sust (And listaASAs) x exp  = anyaMap And listaASAs x exp
sust (Or listaASAs) x exp   = anyaMap Or listaASAs x exp
sust (Add listaASAs) x exp  = anyaMap Add listaASAs x exp
sust (Sub listaASAs) x exp  = anyaMap Sub listaASAs x exp
sust (Mul listaASAs) x exp  = anyaMap Mul listaASAs x exp
sust (Div listaASAs) x exp  = anyaMap Div listaASAs x exp
sust (Lt listaASAs) x exp   = anyaMap Lt listaASAs x exp
sust (Gt listaASAs) x exp   = anyaMap Gt listaASAs x exp
sust (Le listaASAs) x exp   = anyaMap Le listaASAs x exp
sust (Ge listaASAs) x exp   = anyaMap Ge listaASAs x exp
-- en mi mente se veía más limpio :(

{-
sust (Or listaASAs) x exp   = Or (map(\e -> sust e x exp) listaASAs)
sust (Add listaASAs) x exp  = Add (map(\e -> sust e x exp) listaASAs)
sust (Sub listaASAs) x exp  = Sub (map(\e -> sust e x exp) listaASAs)
sust (Mul listaASAs) x exp  = Mul (map(\e -> sust e x exp) listaASAs)
sust (Div listaASAs) x exp  = Div (map(\e -> sust e x exp) listaASAs)
sust (Lt listaASAs) x exp   = Lt (map(\e -> sust e x exp) listaASAs)
sust (Gt listaASAs) x exp   = Gt (map(\e -> sust e x exp) listaASAs)
sust (Le listaASAs) x exp   = Le (map(\e -> sust e x exp) listaASAs)
sust (Ge listaASAs) x exp   = Ge (map(\e -> sust e x exp) listaASAs)
-}

sust (Let bindings e) varObjetivo nuevaExpr =
    let 
        variablesDeclaradas = map fst bindings -- en lugar de \(x, _) -> x, se puede usar fst pues fst :: () => (a, b) -> a (ref: https://hoogle.haskell.org/?hoogle=fst&scope=set%3Astackage), aunque la lambda era más haskell-idiomática. No sabia que había una clase de duplas () 0_0
        sustitucionBindings = map (\(x, expresiones) -> (x, sust expresiones varObjetivo nuevaExpr)) bindings
        sustitucionCuerpo =
            if(elem varObjetivo variablesDeclaradas)
                then e -- sepa dios de donde sacó una función que comprueba si algo está en una lista, y tampoco es necesario hacerla pues elem ya hace eso con una firma muy bonita elem :: Eq a => a -> [a] -> Bool (ref: https://hoogle.haskell.org/?hoogle=elem)
                else (sust e varObjetivo nuevaExpr)
    in (Let sustitucionBindings sustitucionCuerpo)

sust (LetStar [] e) varObjetivo nuevaExpr = LetStar [] (sust e varObjetivo nuevaExpr)
sust (LetStar ((x, exp): xs) e) varObjetivo nuevaExpr =
    let exp = sust exp varObjetivo nuevaExpr -- en una lista (x : exp) el identificador general debe mantenerse consistente
    in if x == varObjetivo then -- la funcion (==) debe estar separada para leer lo siguiente
        LetStar ((x, exp) : xs) e -- supongo que queria hacer recursion para listas dentro de listas
        else case sust (LetStar xs e) varObjetivo nuevaExpr of -- otro error en recursion
        LetStar expr1 e1 -> LetStar ((x, exp): xs) e1 -- supongo que queria hacer recursion en listas dentro de listas, aunque creo solo tendremos listas simples de la forma [a] y no [[a]]
        _ -> error "Esta mal en la sustitucion de Let* :(" -- creo debería mostrar la constante que definimos en el grammars, pero da error, me parece que á firma aquí debe ser error :: HasCallStack => Text -> a (sino quizás deba de enlazarse de una manera mistica) ref: https://hoogle.haskell.org/?hoogle=error&scope=set%3Astackage

--sustMany :: ASA -> [Binding] -> ASA

-- RETO 4: semantica operacional de paso grande
-- let es simultaneo; let* se evalua directamente, asociacion por asociacion.
-- "Amy"
{-
bigStep :: ASA -> Maybe ASA
bigStep (Num n)             = Just $ Num n
bigStep (Boolean b)         = Just $ Boolean b
bigStep (Id x)              = Nothing -- segun lo que vimos con Leslie
bigStep (Let bindings cuerpo) = do -- se debe tener el prelude
    let (xs, exprs) = unzip bindings
    vals <- mapM bigStep exprs
    let nuevoCuerpo = sustMany cuerpo $ zip xs vals
    bigStep nuevoCuerpo -- paso recursivo
bigStep (LetStar [] cuerpo) = bigStep cuerpo
bigStep (LetStar ((x, e1) : cs) cuerpo) = do
    v1 <- bigStep e1
    let colaLetStar = sust (LetStar cs cuerpo) x v1
-}
