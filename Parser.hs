module Parser where

import Prelude hiding ((<*>), (<$>), (<$), (<*), (*>))

import MyScanner
import ParserLibrary
import Contexto
--import AST

parse s = if null lst then error "error de parsing"
                      else fst (head lst)
   where lst = gramatica s

gramatica = sem_Raiz_Raiz <$> pClase

pClase = sem_Class_Class <$ pPalabraReservada "class" <*> pIdentificador <*> pCuerpoClase

pCuerpoClase = sem_CuerpoClase_CuerpoClase <$ pSeparador "{" <*> pMiembros <* pSeparador "}"

pMiembros =  sem_Miembros_MCons <$> pMiembro <*> pMiembros
          <|> succeed sem_Miembros_MNil

{-
pMiembro =  sem_Miembro_Atributo <$> pTipo <*> pIdentificador <* pSeparador ";"
        <|> sem_Miembro_Metodo <$> pTipo <*> pIdentificador <* pSeparador "(" <*> option (listOf pParametro (pSeparador ",")) [] <* pSeparador ")" <*> pCuerpoMetodo
-}
-----------------------------------------------------------------------------
{-Cambiamos la lista de parametros-}
pMiembro =  sem_Miembro_Atributo <$> pTipo <*> pIdentificador <* pSeparador ";"
        <|> sem_Miembro_Metodo <$> pTipo <*> pIdentificador <* pSeparador "(" <*> (option pParametros sem_Ps_Nil) <* pSeparador ")" <*> pCuerpoMetodo

--agregamos parser para parametros

pParametros = sem_Ps_Cons <$> pParametro <*> succeed sem_Ps_Nil
          <|> sem_Ps_Cons <$> pParametro <* pSeparador "," <*> pParametros   

pParametro = sem_Parametro_Parametro <$> pTipo <*> pIdentificador
-----------------------------------------------------------------------------
pCuerpoMetodo = pBloque

{-Cambianos la lista de Instrucciones-}


{-
pBloque = pSeparador "{" *> many (pDeclaracionBloque) <* pSeparador "}"
--- Comentamos para hacer nuestra gramatica de atributos sencilla --
pDeclaracionBloque = pAsignacion 
--                 <|> pCondicional
--                 <|> pWhile
--                 <|> pReturn
-}
{-
pBloque = pSeparador "{" *> (option pDeclaraciones sem_Bs_Nil) <* pSeparador "}"
pDeclaraciones = sem_Bs_Cons <$> pDeclaracionBloque <*> pDeclaraciones
pDeclaracionBloque = pAsignacion
-}
pBloque = pSeparador "{" *> (option pDeclaraciones sem_Bs_Nil) <* pSeparador "}"
pDeclaraciones = sem_Bs_Cons <$> pDeclaracionBloque <*> pDeclaraciones
              <|> succeed sem_Bs_Nil 
pDeclaracionBloque = pAsignacion

-------------------------------------------------------------------------------
{-
pCondicional =  If <$ pPalabraReservada "if" <* pSeparador "(" <*> pExpresionCondicional <* pSeparador ")" <* pSeparador "{" <*> many (pDeclaracionBloque) <* pSeparador "}" <*> (option (Else <$ pPalabraReservada "else" <* pSeparador "{" <*> many (pDeclaracionBloque) <* pSeparador "}") NoElse) 
-}

--pReturn = Return <$ pPalabraReservada "return" <*> pExpresionCondicional <* pSeparador ";"

--pCondicional =  If <$ pPalabraReservada "if" <* pSeparador "(" <*> pExpresionCondicional <* pSeparador ")" <*> pBloque
-- <|> IfElse <$ pPalabraReservada "if" <* pSeparador "(" <*> pExpresionCondicional <* pSeparador ")" <*> pBloque <* pPalabraReservada "else" <*> pElse 

--pElse = Else <$> pBloque
--    <|> ElseIf <$> pCondicional
    
{-    
pWhile =  While <$ pPalabraReservada "while" <* pSeparador "(" <*> pExpresionCondicional <* pSeparador ")" <* pSeparador "{" <*> many (pDeclaracionBloque) <* pSeparador "}" 
-}
--pWhile =  While <$ pPalabraReservada "while" <* pSeparador "(" <*> pExpresionCondicional <* pSeparador ")" <*> pBloque 
                
pAsignacion = sem_DeclaracionBloque_Asignacion <$> pIdentificador <* pOperador "=" <*> pExpresionCondicional <* pSeparador ";"

pExpresionCondicional = (sem_Expresion_Operacion sem_Operador_O) <$> pExpresionRelacional <* pOperador "||" <*> pExpresionCondicional 
                    <|> (sem_Expresion_Operacion sem_Operador_Y) <$> pExpresionRelacional <* pOperador "&&" <*> pExpresionCondicional 
                    <|> pExpresionRelacional

pExpresionRelacional = (sem_Expresion_Operacion sem_Operador_Igl) <$> pExpresion <* pOperador "==" <*> pExpresionRelacional 
                   <|> (sem_Expresion_Operacion sem_Operador_Dif) <$> pExpresion <* pOperador "!=" <*> pExpresionRelacional 
                   <|> (sem_Expresion_Operacion sem_Operador_Men) <$> pExpresion <* pOperador "<" <*> pExpresionRelacional 
                   <|> (sem_Expresion_Operacion sem_Operador_May) <$> pExpresion <* pOperador ">" <*> pExpresionRelacional 
                   <|> pExpresion
{-
pExpresion = chainr pTermino (   const (sem_Expresion_Operacion sem_Operador_Sum) <$> pOperador "+"
                             <|> const (sem_Expresion_Operacion sem_Operador_Res) <$> pOperador "-"
                             )
                             
pTermino = chainr pFactor (  const (sem_Expresion_Operacion sem_Operador_Mul) <$> pOperador "*"
                         <|> const (sem_Expresion_Operacion sem_Operador_Mod) <$> pOperador "%"
                          <|> const (sem_Expresion_Operacion sem_Operador_Div) <$> pOperador "/"
                          )
-}
pExpresion = (sem_Expresion_Operacion sem_Operador_Sum) <$> pTermino <* pOperador "+" <*> pExpresion
         <|> (sem_Expresion_Operacion sem_Operador_Res) <$> pTermino <* pOperador "-" <*> pExpresion
         <|> pTermino
 
pTermino =  (sem_Expresion_Operacion sem_Operador_Mul) <$> pFactor <* pOperador "*" <*> pTermino <|> pFactor
         
pFactor = sem_Expresion_Con <$> pLiteral 
      <|> sem_Expresion_Var <$> pIdentificador
      <|> pFuncion 
      <|> pSeparador "(" *> pExpresion <* pSeparador ")"

-------------------------------------------
{-Cambiamos la lista de expresiones-}
{-
pFuncion = sem_Expresion_Fun <$> pIdentificador <* pSeparador "(" <*> option (listOf pExpresion (pSeparador ",")) [] <* pSeparador ")"
-}
pFuncion = sem_Expresion_Fun <$> pIdentificador <* pSeparador "(" <*> (option pExpresiones sem_Es_Nil) <* pSeparador ")"

--agregamos parser para expresiones
pExpresiones = sem_Es_Cons <$> pExpresion <*> succeed sem_Es_Nil
           <|> sem_Es_Cons <$> pExpresion <* pSeparador "," <*> pExpresiones
---------------------------------------------------------------------------------

pLiteral = sem_Literal_LiteralEntero <$> pInteger 
       <|> sem_Literal_LiteralBooleano <$> pBoolean
       
pInteger = (\(IntegerLiteral n) -> (read n::Int)) <$> symbol (IntegerLiteral "")

pSuma :: Parser Symbol (Integer -> Integer -> Integer)
pSuma = (\_ -> (+)) <$> pOperador "+" 

pMult = (\_ -> (*)) <$> pOperador "*"


pBoolean = (\(BooleanLiteral b) -> (if b == "true" then True else False)) <$> symbol (BooleanLiteral "")

pPalabraReservada :: String -> Parser Symbol Symbol
pPalabraReservada s = symbol (ReservedWord s)

pSeparador s = symbol (Separator s)

pIdentificador :: Parser Symbol String
pIdentificador =  (\(Identifier i) -> i) <$> symbol (Identifier "")

pOperador s = symbol (Operator s)

--pTipo :: Parser Symbol Tipo
pTipo =   sem_Tipo_TEntero   <$ pPalabraReservada "int"
    <|> sem_Tipo_TBooleano <$ pPalabraReservada "boolean" 
    <|> sem_Tipo_TVoid <$ pPalabraReservada "void"
      
---Funciones semanticas

