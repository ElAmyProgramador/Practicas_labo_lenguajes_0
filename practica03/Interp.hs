module Interp where

import Grammars

-- RETO 3: sustitucion nominal que evita captura -- Estefany
--freeVars :: ASA -> [String]

--names :: ASA -> [String]

--freshName :: [String] -> String

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
-- utilizamos map para aplicar la función a cada element de cada ASA
-- esta funcion se hace para evitar reescribir código
--anyaMap :: ASA -> String -> ASA -> ASA
--anyaMap (asaF lstASA) y exp = asaF $ map (\e -> sust e y exp) lstASA
sust (And listaASAs) x exp  = And (map(\e -> sust e x exp) listaASAs)
sust (Or listaASAs) x exp   = Or (map(\e -> sust e x exp) listaASAs)
sust (Add listaASAs) x exp  = Add (map(\e -> sust e x exp) listaASAs)
sust (Sub listaASAs) x exp  = Sub (map(\e -> sust e x exp) listaASAs)
sust (Mul listaASAs) x exp  = Mul (map(\e -> sust e x exp) listaASAs)
sust (Div listaASAs) x exp  = Div (map(\e -> sust e x exp) listaASAs)
sust (Lt listaASAs) x exp   = Lt (map(\e -> sust e x exp) listaASAs)
sust (Gt listaASAs) x exp   = Gt (map(\e -> sust e x exp) listaASAs)
sust (Le listaASAs) x exp   = Le (map(\e -> sust e x exp) listaASAs)
sust (Ge listaASAs) x exp   = Ge (map(\e -> sust e x exp) listaASAs)

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
