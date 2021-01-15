module AST where
-- Abstract syntax grammar

data Class = Class String CuerpoClase
           deriving Show

sem_Class_Class = Class
----------------------------------------------
data CuerpoClase = CuerpoClase Miembros
                deriving Show

sem_CuerpoClase_CuerpoClase = CuerpoClase
----------------------------------------------
data Miembros = MCons Miembro Miembros
              | MNil
              deriving Show

sem_Miembros_MCons = MCons
sem_Miembros_MNil  = MNil
-----------------------------------------------
data Miembro = Atributo Tipo String
             | Metodo Tipo String Ps Bs
             deriving Show
             
sem_Miembro_Atributo = Atributo
sem_Miembro_Metodo   = Metodo             
------------------------------------------------             
type Ps = [Parametro]
----------------------------------------------
data Parametro = Parametro Tipo String
               deriving Show
            
sem_Parametro_Parametro = Parametro
----------------------------------------------              
type Bs = [DeclaracionBloque]
                                          
data DeclaracionBloque = Asignacion String Expresion
--                       | If Expresion Bs
--                       | IfElse Expresion Bs DeclaracionBloque
--                       | Else Bs
--                       | ElseIf DeclaracionBloque
--                       | While Expresion Bs
--                       | Return Expresion
                      deriving Show

sem_DeclaracionBloque_Asignacion = Asignacion
-------------------------------------------------------                      
{-
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
-}
data Expresion = Con Literal
               | Var String
               | Fun String Es
               | Sum Expresion Expresion
               | Res Expresion Expresion
               | Mul Expresion Expresion
               | Mod Expresion Expresion
               | Div Expresion Expresion
               | O   Expresion Expresion
               | Y   Expresion Expresion
               | Igl Expresion Expresion
               | Dif Expresion Expresion
               | Men Expresion Expresion
               | May Expresion Expresion
               | MenIgl Expresion Expresion
               | MayIgl Expresion Expresion
               deriving Show
               
type Es = [Expresion]

sem_Expresion_Con = Con
sem_Expresion_Var = Var
sem_Expresion_Fun = Fun

---------------------------------------------------------
--data InizializadorVariable = InizializadorLiteral Literal
--          | InizializadorIdentificador String
--          | NoInizializado             
--          deriving Show

data Literal = LiteralEntero Int
             | LiteralBoolean Bool
             deriving Show
                 
data Tipo = TEntero 
          | TBooleano 
          | TVoid
          deriving Show

--semantic functions

