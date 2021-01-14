module Parser where

import Prelude hiding ((<*>), (<$>), (<$), (<*), (*>))

import MyScanner
import ParserLibrary

import AST

parse = gramatica

gramatica = pClase

pClase = Class <$ pPalabraReservada "class" <*> pIdentificador <*> pCuerpoClase

pCuerpoClase = CuerpoClase <$ pSeparador "{" <*> pMiembros <* pSeparador "}"

pMiembros =   MCons <$> pMiembro <*> pMiembros
          <|> succeed MNil

pMiembro =  Atributo <$> pTipo <*> pIdentificador <* pSeparador ";"
        <|> Metodo   <$> pTipo <*> pIdentificador <* pSeparador "(" <*> option (listOf pParametro (pSeparador ",")) [] <* pSeparador ")" <*> pCuerpoMetodo

pParametro = Parametro <$> pTipo <*> pIdentificador

pCuerpoMetodo = pBloque

pBloque = pSeparador "{" *> many (pDeclaracionBloque) <* pSeparador "}"

pDeclaracionBloque = pAsignacion 
                 <|> pCondicional
                 <|> pWhile
                 <|> pReturn
{-
pCondicional =  If <$ pPalabraReservada "if" <* pSeparador "(" <*> pExpresionCondicional <* pSeparador ")" <* pSeparador "{" <*> many (pDeclaracionBloque) <* pSeparador "}" <*> (option (Else <$ pPalabraReservada "else" <* pSeparador "{" <*> many (pDeclaracionBloque) <* pSeparador "}") NoElse) 
-}
pReturn = Return <$ pPalabraReservada "return" <*> pExpresionCondicional <* pSeparador ";"

pCondicional =  If <$ pPalabraReservada "if" <* pSeparador "(" <*> pExpresionCondicional <* pSeparador ")" <*> pBloque
 <|> IfElse <$ pPalabraReservada "if" <* pSeparador "(" <*> pExpresionCondicional <* pSeparador ")" <*> pBloque <* pPalabraReservada "else" <*> pElse 

pElse = Else <$> pBloque
    <|> ElseIf <$> pCondicional
    
{-    
pWhile =  While <$ pPalabraReservada "while" <* pSeparador "(" <*> pExpresionCondicional <* pSeparador ")" <* pSeparador "{" <*> many (pDeclaracionBloque) <* pSeparador "}" 
-}
pWhile =  While <$ pPalabraReservada "while" <* pSeparador "(" <*> pExpresionCondicional <* pSeparador ")" <*> pBloque 
                
pAsignacion = Asignacion <$> pIdentificador <* pOperador "=" <*> pExpresionCondicional <* pSeparador ";"

pExpresionCondicional = (:||:) <$> pExpresionRelacional <* pOperador "||" <*> pExpresionCondicional <|> (:&&:) <$> pExpresionRelacional <* pOperador "&&" <*> pExpresionCondicional <|> pExpresionRelacional

pExpresionRelacional = (:==:) <$> pExpresion <* pOperador "==" <*> pExpresionRelacional <|> (:!=:) <$> pExpresion <* pOperador "!=" <*> pExpresionRelacional <|> (:<:) <$> pExpresion <* pOperador "<" <*> pExpresionRelacional <|> (:>:) <$> pExpresion <* pOperador ">" <*> pExpresionRelacional <|> pExpresion

pExpresion = chainr pTermino (   const (:+:) <$> pOperador "+"
                             <|> const (:-:) <$> pOperador "-"
                             )
pTermino = chainr pFactor (   const (:*:) <$> pOperador "*"
                          <|> const (:%:) <$> pOperador "%"
                          <|> const (:/:) <$> pOperador "/"
                          )

pFactor = Con <$> pLiteral 
      <|> Var <$> pIdentificador 
      <|> pFuncion 
      <|> pSeparador "(" *> pExpresion <* pSeparador ")"

pFuncion = Fun <$> pIdentificador <* pSeparador "(" <*> option (listOf pExpresion (pSeparador ",")) [] <* pSeparador ")"


pLiteral = LiteralEntero <$> pInteger 
       <|> LiteralBoolean <$> pBoolean

pInteger = (\(IntegerLiteral n) -> (read n::Int)) <$> symbol (IntegerLiteral "")

pSuma :: Parser Symbol (Integer -> Integer -> Integer)
pSuma = (\_ -> (+)) <$> pOperador "+" 

pMult = (\_ -> (*)) <$> pOperador "*"

{-
pInteger' = (\(IntegerLiteral i) -> (read i::Integer)) <$> symbol (IntegerLiteral "")

p1 = chainr p2 pSuma

p2 = chainr pInteger' pMult

p3 = chainr (chainr pInteger' pMult) pSuma

p4 = ( pInteger' `chainr` pMult ) `chainr` pSuma

p5 = foldl chainr pInteger' [pMult, pSuma]

p6 = chainr pInteger' pSuma

--( pInteger' `chainr` pMult ) `chainr` pSuma
 
ejemplo1 = [IntegerLiteral "4", Operator "+", IntegerLiteral "6", Operator "+", IntegerLiteral "5"]
ejemplo2 = [IntegerLiteral "4", Operator "+", IntegerLiteral "6", Operator "*", IntegerLiteral "5"]
ejemplo3 = [IntegerLiteral "4", Operator "+", IntegerLiteral "5"]
ejemplo4 = [IntegerLiteral "5", Operator "*", IntegerLiteral "6", Operator "+", IntegerLiteral "3", Operator "*", IntegerLiteral "2"]
-}
pBoolean = (\(BooleanLiteral b) -> (if b == "true" then True else False)) <$> symbol (BooleanLiteral "")

-- Abstract syntax grammar
{-
data Class = Class String CuerpoClase
           deriving Show

data CuerpoClase = CuerpoClase Miembros
                deriving Show

data Miembros = MCons Miembro Miembros
              | MNil
              deriving Show

data Miembro = Atributo Tipo String
             | Metodo Tipo String Ps Bs
             deriving Show
             
type Ps = [Parametro]

data Parametro = Parametro Tipo String
               deriving Show
              
type Bs = [DeclaracionBloque]
                                          
data DeclaracionBloque = Asignacion String Expresion
                       | If Expresion Bs
                       | IfElse Expresion Bs DeclaracionBloque
                       | Else Bs
                       | ElseIf DeclaracionBloque
                       | While Expresion Bs
                       | Return Expresion
                      deriving Show
                      
data Expresion = Con Int
               | Var String
               | Fun String Es
               | Expresion  :+:  Expresion
               | Expresion  :-:  Expresion
               | Expresion  :*:  Expresion
               | Expresion  :%:  Expresion
               | Expresion  :/:  Expresion
               | Expresion  :||: Expresion
               | Expresion  :&&: Expresion
               | Expresion  :==: Expresion
               | Expresion  :!=: Expresion
               | Expresion  :<:  Expresion
               | Expresion  :>:  Expresion
               | Expresion  :<=: Expresion
               | Expresion  :>=: Expresion
               deriving Show
type Es = [Expresion]

data InizializadorVariable = InizializadorLiteral Literal
          | InizializadorIdentificador String
          | NoInizializado             
          deriving Show

data Literal = LiteralEntero Int
             | LiteralBoolean Bool
             deriving Show
                 
data Tipo = TEntero 
          | TBooleano 
          | TVoid
          deriving Show
-}

pPalabraReservada :: String -> Parser Symbol Symbol
pPalabraReservada s = symbol (ReservedWord s)

pSeparador s = symbol (Separator s)

pIdentificador :: Parser Symbol String
pIdentificador =  (\(Identifier i) -> i) <$> symbol (Identifier "")

pOperador s = symbol (Operator s)

pTipo :: Parser Symbol Tipo
pTipo =   TEntero   <$ pPalabraReservada "int"
      <|> TBooleano <$ pPalabraReservada "boolean" <|> TVoid <$ pPalabraReservada "void"
      


