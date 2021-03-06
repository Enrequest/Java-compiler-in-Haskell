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

pParametro = Parametro <$> pTipo <*> pIdentificador

pCuerpoMetodo = Cuerpo <$> pBloque <|> CuerpoNil <$ pSeparador ";"

{-
pBloque = pSeparador "{" *> option (Bloque <$> pDeclaracionesBloque) BloqueNil <* pSeparador "}" 
-}

pBloque = pSeparador "{" *> Bloque <$> many (pDeclaracionBloque) <* pSeparador "}"

pDeclaracionBloque = DeclaracionVariableLocal <$> pVariableLocal <|> DeclaracionAsignacion <$> pAsignacion

pVariableLocal = VariableLocal <$> pTipo <*> pListaVariablesLocal <* pSeparador ";"

pListaVariablesLocal = ListaVariablesLocal <$> pDeclaradorVariable <*> many ( pSeparador "," *> pDeclaradorVariable)

pDeclaradorVariable = DeclaradorVariable <$> pIdentificador <*> option (pOperador "=" *> pInizializadorVariable) NoInizializado

pAsignacion = Asignacion <$> pIdentificador <* pOperador "=" <*> pInizializadorVariable <* pSeparador ";"

pInizializadorVariable = InizializadorLiteral <$> pLiteral <|> InizializadorIdentificador <$> pIdentificador

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
             
data DeclaracionBloque = DeclaracionVariableLocal VariableLocal
                       | DeclaracionAsignacion Asignacion
                     deriving Show
                     
data VariableLocal = VariableLocal Tipo ListaVariablesLocal 
                   deriving Show
                  
data ListaVariablesLocal = 
    ListaVariablesLocal DeclaradorVariable [DeclaradorVariable]
    deriving Show

data DeclaradorVariable = DeclaradorVariable String InizializadorVariable
                        deriving Show

data Asignacion = Asignacion String InizializadorVariable
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
      






{-
pDeclaracionesBloque = DeclaracionesBloque <$> pDeclaracionBloque <*> many pDeclaracionBloque

pDeclaracionBloque = DeclaracionBloqueVariable<$> pDeclaracionVariable <|> DeclaracionBloque <$> pDeclaracion

pDeclaracionVariable = DeclaracionVariable <$> pTipo <*> pListaVariablesLocal <* pSeparador ";"

pDeclaracion = Declaracion <$> pDeclaracionExpresion <* pSeparador ";"

pDeclaracionExpresion = DeclaracionExpresion <$> pAsignacion
-}
       
{-
data Bloque = Bloque DeclaracionesBloque
              | BloqueNil
            deriving Show
                        
data DeclaracionesBloque = DeclaracionesBloque DeclaracionBloque [DeclaracionBloque]
                 deriving Show
                  
data DeclaracionBloque = 
                DeclaracionBloqueVariable DeclaracionVariableLocal
              | DeclaracionBloque Declaracion
              | FinDeclaracion
                  deriving Show
                                      
data DeclaracionVariableLocal = 
               DeclaracionVariable Tipo ListaVariablesLocal
                         deriving Show

data ListaVariablesLocal = 
    ListaVariablesLocal DeclaradorVariable [DeclaradorVariable]
    deriving Show
    
data DeclaradorVariable = DeclaradorVariable String InizializadorVariable
                        deriving Show
                                                             
data Declaracion = Declaracion DeclaracionExpresion
                 deriving Show
                 
data DeclaracionExpresion = DeclaracionExpresion Asignacion
                      deriving Show
-}      

