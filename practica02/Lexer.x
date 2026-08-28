-- Reto 1 y 3 "Amy"

{
module Lexer (Token(..), lexer) where

import Data.Char (isSpace)
}

-- Esta wea hace que el Scanner sea de tipo Scanner :: String -> [Token] (creo es más bien una clase de tipo)
%wrapper "basic" -- Seria interesante usar la API en su forma de más bajo nivel

-- Macros para los tokens
$white = [\x20\x09\x0A\x0D\x0C\x0B]
$digit = 0-9
$nonzero = 1-9

-- Los naturales hijos de su inductiva madre ojalá nunca hubiesen existido -_-
@nat = 0 | $nonzero $digit*

-- Definición de tokens para comprar nft's :3 (por medio del Scanner)
tokens :-

  $white+               ; -- Esto es una regla que hace match dependiendo de los espacios, si es $white* al parecer tambien cuenta los no espacios

  -- Tokens ya presentes en MINILISP01
  \(                    { \_ -> TokenPA }
  \)                    { \_ -> TokenPC }
  \+                    { \_ -> TokenSuma }
  \-                    { \_ -> TokenResta }
  not                   { \_ -> TokenNot }

  "#t"                  { \_ -> TokenBool True }
  "#f"                  { \_ -> TokenBool False }

  0$digit+              { \s -> error ("Lexical error: natural con cero inicial = "
                                      ++ show s) }
  @nat                  { \s -> TokenNum (read s) }

  -- RETO 1:
  -- Agrega aqui las reglas lexicas para:
  --   and, or, *, /, expt, <, >, <=, >=, eq, add1, sub1, zero?
  -- Recuerda reconocer <= y >= como tokens completos.

  and                   { \_ -> TokenAnd }
  or                    { \_ -> TokenOr }
  \*                    { \_ -> TokenMul }
  \/                    { \_ -> TokenDiv }
  expt                  { \_ -> TokenExpt }
  \<=                   { \_ -> TokenLE }
  \>=                   { \_ -> TokenGE }
  \<                    { \_ -> TokenLT }
  \>                    { \_ -> TokenGT }
  eq                    { \_ -> TokenEq }
  add1                  { \_ -> TokenAdd1 }
  sub1                  { \_ -> TokenSub1 }
  zero\?                { \_ -> TokenZeroP } -- Aunque usa el white encoding UTF-8, el '?' debe ponerse como '\?'

  -- Por alguna razón esto debe ir al final, creo es por el 
  .                     { \s -> error ("Lexical error: caracter no reconocido = "
                                      ++ show s
                                      ++ " | codepoints = "
                                      ++ show (map fromEnum s))
  }

-- Hecho por el dato alex := [ @code ] [ wrapper ] [ encoding ] { macrodef } @id ':-' { rule } [ @code ]

{
data Token
  = TokenNum Int
  | TokenBool Bool
  | TokenSuma
  | TokenResta
  | TokenMul
  | TokenDiv
  | TokenAnd
  | TokenOr
  | TokenNot
  | TokenAdd1
  | TokenSub1
  | TokenZeroP
  | TokenExpt
  | TokenLT
  | TokenGT
  | TokenLE
  | TokenGE
  | TokenEq
  | TokenPA
  | TokenPC
  deriving (Eq, Show)

normalizeSpaces :: String -> String
normalizeSpaces = map (\c -> if isSpace c then '\x20' else c)

lexer :: String -> [Token]
lexer = alexScanTokens . normalizeSpaces
}
