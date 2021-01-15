module Main where

import MyScanner
import Parser
import Contexto


main = do programa <- readFile "Ejemplo.java"
          let simbolos = trozar programa
          let resultado = parse simbolos
          putStrLn resultado


                 
