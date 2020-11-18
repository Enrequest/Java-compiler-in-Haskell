module Parser where

import Prelude hiding ((<*>), (<$>), (<$), (<*), (*>))

import MyScanner
import ParserLibrary

parse = gramatica

gramatica = pClase

pClase = Class <$ pPalabraReservada "class" <*> pIdentificador <*> pCuerpoClase

pCuerpoClase = CuerpoClase <$ pSeparador "{" <*> pMiembros <* pSeparador "}"

--pClass = Class <$ pPalabraReservada "class"
--problems here

pMiembros =   MCons <$> pMiembro <*> pMiembros
          <|> succeed MNil

pMiembro = Atributo <$> pTipo <*> pIdentificador <* pSeparador ";" <|> Metodo <$> pTipo <*> pIdentificador <* pSeparador "(" <*> (option pParametros PNil) <* pSeparador ")" <*> pCuerpoMetodo

{-
pParametros = ParametroFinal <$> pParametro <|> Parametros <$> pParametro <* pSeparador "," <*> pParametros
-}

pParametros = Parametros <$> pParametro <*> succeed PNil <|> Parametros <$> pParametro <* pSeparador "," <*> pParametros

{-
pParametros = foldl f c <$> many1 (pParametro <* pSeparador "," <*> pParametros)
       where f a b = Parametros b a
             c = PNil
-}

pParametro = Parametro <$> pTipo <*> pIdentificador

pCuerpoMetodo = Cuerpo <$> pBloque <|> CuerpoNil <$ pSeparador ";"

{-
pBloque = pSeparador "{" *> option (Bloque <$> pDeclaracionesBloque) BloqueNil <* pSeparador "}" 
-}

pBloque = pSeparador "{" *> Bloque <$> many (pDeclaracionBloque) <* pSeparador "}"

pDeclaracionBloque = DeclaracionAsignacion <$> pAsignacion

{-
pDeclaradorVariable = DeclaradorVariable <$> pIdentificador <*> option (pOperador "=" *> pInizializadorVariable) NoInizializado
-}
{-
pAsignacion = Asignacion <$> pIdentificador <* pOperador "=" <*> pExpresion <* pSeparador ";"
-}

pAsignacion = Asignacion <$> pIdentificador <* pOperador "=" <*> pExpresionCondicional <* pSeparador ";"

pExpresionCondicional = Or <$> (ExpR <$> pExpresionRelacional) <* pOperador "||" <*> pExpresionCondicional <|> And <$> (ExpR <$> pExpresionRelacional) <* pOperador "&&" <*> pExpresionCondicional <|> ExpR <$> pExpresionRelacional

pExpresionRelacional = Igual <$> pExpresion <* pOperador "==" <*> pExpresionRelacional <|> NIgual <$> pExpresion <* pOperador "!=" <*> pExpresionRelacional <|> Menor <$> pExpresion <* pOperador "<" <*> pExpresionRelacional <|> Mayor <$> pExpresion <* pOperador ">" <*> pExpresionRelacional <|> Expr <$> pExpresion

pExpresion  = Suma <$> pTermino <* pOperador "+" <*> pExpresion <|> Resta <$> pTermino <* pOperador "-" <*> pExpresion <|> pTermino

pTermino = Producto <$> pFactor <* pOperador "*" <*> pTermino <|> pFactor

pFactor = Con <$> pInteger <|> Var <$> pIdentificador <|> pFuncion <|> pSeparador "(" *> pExpresion <* pSeparador ")"

pFuncion = Fun <$> pIdentificador <* pSeparador "(" <*> (option pArgumentos []) <* pSeparador ")"

pArgumentos = list <$> pExpresion <*> many (pSeparador "," *> pExpresion) 

{-
pInizializadorVariable = InizializadorLiteral <$> pLiteral <|> InizializadorIdentificador <$> pIdentificador
-}

pLiteral = LiteralEntero <$> pInteger <|> LiteralBoolean <$> pBoolean

pInteger = (\(IntegerLiteral n) -> (read n::Int)) <$> symbol (IntegerLiteral "")

pBoolean = (\(BooleanLiteral b) -> (if b == "true" then True else False)) <$> symbol (BooleanLiteral "")

-- Abstract syntax grammar

data Class = Class String CuerpoClase
           deriving Show

data CuerpoClase = CuerpoClase Miembros
                deriving Show

data Miembros = MCons Miembro Miembros
              | MNil
              deriving Show

data Miembro = Atributo Tipo String
             | Metodo Tipo String Parametros CuerpoMetodo
             deriving Show

data Parametros = Parametros Parametro Parametros
                | PNil
                deriving Show

data Parametro = Parametro Tipo String
               deriving Show

data CuerpoMetodo = Cuerpo Bloque | CuerpoNil
                  deriving Show

data Bloque = Bloque [DeclaracionBloque]
             deriving Show
{-             
data DeclaracionBloque = DeclaracionVariableLocal VariableLocal
                       | DeclaracionAsignacion Asignacion
                     deriving Show
-}

data DeclaracionBloque = DeclaracionAsignacion Asignacion
                     deriving Show
                     
{-                     
data VariableLocal = VariableLocal Tipo ListaVariablesLocal 
                   deriving Show
                                     
data ListaVariablesLocal = 
    ListaVariablesLocal DeclaradorVariable [DeclaradorVariable]
    deriving Show
-}

data DeclaradorVariable = DeclaradorVariable String InizializadorVariable
                        deriving Show
{-
data Asignacion = Asignacion String Expresion
                 deriving Show
-}
data Asignacion = Asignacion String ExpresionCondicional
                 deriving Show

data ExpresionCondicional = Or ExpresionCondicional ExpresionCondicional
                          | And ExpresionCondicional ExpresionCondicional
                          | ExpR ExpresionRelacional
                          deriving Show

data ExpresionRelacional  = Igual Expresion ExpresionRelacional
                          | NIgual Expresion ExpresionRelacional
                          | Menor Expresion ExpresionRelacional
                          | Mayor Expresion ExpresionRelacional
                          | Expr Expresion
                          deriving Show
                          
data Expresion = Con Int
               | Var String
               | Fun String [Expresion]
               | Suma Expresion Expresion
               | Resta Expresion Expresion
               | Producto Expresion Expresion 
               deriving Show

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

pPalabraReservada :: String -> Parser Symbol Symbol
pPalabraReservada s = symbol (ReservedWord s)

pSeparador s = symbol (Separator s)

pIdentificador :: Parser Symbol String
pIdentificador =  (\(Identifier i) -> i) <$> symbol (Identifier "")

--pOperador :: String -> Parser Symbol Symbol
pOperador s = symbol (Operator s)

pTipo :: Parser Symbol Tipo
pTipo =   TEntero   <$ pPalabraReservada "int"
      <|> TBooleano <$ pPalabraReservada "boolean" <|> TVoid <$ pPalabraReservada "void"
      


