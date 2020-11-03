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

pMiembro = Atributo <$> pTipo <*> pIdentificador <* pSeparador ";" <|> Metodo <$> pTipo <*> pIdentificador <* pSeparador "(" <*> pParametros <* pSeparador ")" <*> pCuerpoMetodo

pParametros = ParametroFinal <$> pParametro <|> Parametros <$> pParametro <* pSeparador "," <*> pParametros <|> succeed PNil

pParametro = Parametro <$> pTipo <*> pIdentificador

pCuerpoMetodo = Cuerpo <$> pBloque <|> CuerpoNil <$ pSeparador ";"

pBloque = Bloque <$ pSeparador "{" <*> pDeclaracionesBloque <* pSeparador "}" <|> BloqueNil <$ pSeparador "{" <* pSeparador "}" 

pDeclaracionesBloque = DeclaracionFinal<$>pDeclaracionBloque <|>  Declaraciones <$> pDeclaracionBloque <*> pDeclaracionesBloque
 
pDeclaracionBloque = DeclaracionBloqueVariable<$> pDeclaracionVariable <|> DeclaracionBloque <$> pDeclaracion

pDeclaracionVariable = DeclaracionVariable <$> pVariableLocal <* pSeparador ";"

pVariableLocal = VariableLocal <$> pTipo <*> pDeclaradoresVariables

pDeclaradoresVariables = VariableFinal <$> pDeclaradorVariable <|> Variables <$> pDeclaradorVariable <* pSeparador "," <*> pDeclaradoresVariables

pDeclaradorVariable = NoInizializada <$> pIdentificador <|> Inizializada <$> pIdentificador <* pOperador "=" <*> pInizializadorVariable

pDeclaracion = Declaracion <$> pDeclaracionExpresion <* pSeparador ";"

pDeclaracionExpresion = DeclaracionExpresion <$> pAsignacion

pAsignacion = Asignacion <$> pIdentificador <* pOperador "=" <*> pInizializadorVariable

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

data Parametros = ParametroFinal Parametro
                | Parametros Parametro Parametros
                | PNil
                deriving Show

data Parametro = Parametro Tipo String
               deriving Show

data CuerpoMetodo = Cuerpo Bloque | CuerpoNil
                  deriving Show

data Bloque = Bloque DeclaracionesBloque
              | BloqueNil
            deriving Show
            
data DeclaracionesBloque = 
                     DeclaracionFinal DeclaracionBloque
                   | Declaraciones DeclaracionBloque DeclaracionesBloque
                   | DNil
                   deriving Show

data DeclaracionBloque = 
                DeclaracionBloqueVariable DeclaracionVariableLocal
              | DeclaracionBloque Declaracion
                  deriving Show
                                      
data DeclaracionVariableLocal = DeclaracionVariable VariableLocal
                         deriving Show

data VariableLocal = VariableLocal Tipo DeclaradoresVariables
                   deriving Show
                   
data DeclaradoresVariables = VariableFinal DeclaradorVariable
                           | Variables DeclaradorVariable DeclaradoresVariables
                           deriving Show
                          
data DeclaradorVariable = NoInizializada String
                  | Inizializada String InizializadorVariable
                        deriving Show         
                                     
data Declaracion = Declaracion DeclaracionExpresion
                 deriving Show
                
data DeclaracionExpresion = DeclaracionExpresion Asignacion
                      deriving Show
                
data Asignacion = Asignacion String InizializadorVariable
                 deriving Show
                 
data InizializadorVariable = InizializadorLiteral Literal
          |InizializadorIdentificador String             
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

