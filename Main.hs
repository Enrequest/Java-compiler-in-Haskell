module Main where

import MyScanner
import Parser

{-
main = do programa <- readFile "Ejemplo.java"
          let simbolos = scanner programa
          let resultado = parse simbolos
          print resultado
-}  


main = do programa <- readFile "Ejemplo.java"
          let simbolos = trozar programa
          let resultado = parse simbolos
          print resultado


{-
main = do programa <- readFile "Ejemplo1.java"
          putStrLn(show (trozar programa))
-}

                 
