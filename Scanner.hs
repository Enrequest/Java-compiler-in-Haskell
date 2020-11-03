module Scanner where
main:: IO()
main = do
        contents <- readFile "Prueba.java"
        let lista = scanner contents
        print lista

data Symbol = ReservedWord   String
            | Separator      Char
            | Operator       String
            | Identifier     String
            | IntegerLiteral String
            | StringLiteral  String
            | CharLiteral    String
        deriving (Show)

scanner:: String -> [Symbol]
scanner [] = []
scanner xs = 
             case rs of 
                "digit"     -> (IntegerLiteral ws):scanner zs
                "reserved"  -> (ReservedWord ws):scanner zs
                "word"      -> (Identifier ws):scanner zs
                "operator"  -> (Operator ws):scanner zs
                "separator" -> (Separator (head ws)):scanner zs
                "string"    -> (StringLiteral ws):scanner zs
                "character" -> (CharLiteral ws):scanner zs
                _ -> scanner zs
             where
                 (rs, ws, zs) = sourceState xs
sourceState:: String -> (String, String, String)
sourceState ys@[] = (ys,ys,ys)
sourceState (x:xs) | isWhitespace x = sourceState xs
                   | isDigit x      = let (str,ws,zs) = digitState xs     in (str,x:ws,zs)
                   | isLetter  x    = let (str,ws,zs) = wordState xs     
                            in if isReservedWord (x:ws) then ("reserved",x:ws,zs)
                               else (str,x:ws,zs)
                   | isSeparator x  = ("separator",[x],xs)
                   | x == '/'       = let (str,ws,zs) = slash xs
                              in if isOperator (x:ws) then ("operator",x:ws,zs)
                                 else if str == "commentary" then (str,[],zs)
                                      else error ("\nInvalid operator "++(x:ws))
                   | isSingleOperator  x  = let (str,ws,zs) = operatorState xs  
                            in if isOperator (x:ws) then ("operator",x:ws,zs)
                               else error ("\nInvalid operator "++(x:ws))
                   | x == '\"'      = let (str,ws,zs) = readStr xs in (str,x:ws,zs)
                   | x == '\''      = let (str,ws,zs) = readChar xs in (str,x:ws,zs) 
                   | otherwise      = error "\nInvalid character\n"

digitState::String -> (String, String, String)
digitState ys@[] = ("digit",ys,ys)
digitState ys@(x:xs) | isLetter x = error "\nIs not a valid identifier\n" 
                     | isDigit x  =  let (str,ws,zs) = digitState xs in (str,x:ws,zs)
                     | otherwise  = ("digit",[], ys)

wordState::String -> (String, String, String)
wordState ys@[] = ("word",ys,ys)
wordState ys@(x:xs) | isWord x  =  let (str,ws,zs) = wordState xs in (str,x:ws,zs)
                    | otherwise = ("word",[],ys)

slash::String -> (String, String, String)
slash ys@(x:xs) | x == '/'  = endOfLineComment xs
                | x == '*'  = commentTail xs
                | otherwise = let (str,ws,zs) = operatorState xs in (str,x:ws,zs)

endOfLineComment::String -> (String, String, String)
endOfLineComment ys@[] = ("commentary",ys,ys)
endOfLineComment ys@(x:xs) | x == '\n' = ("commentary",[],xs)
                           | otherwise = endOfLineComment xs

commentTail::String -> (String, String, String)
commentTail ys@[] = (ys,ys,ys)
commentTail ys@(x:xs) | x == '*'  = commentTailStar xs
                      | otherwise = commentTail xs

commentTailStar::String -> (String, String, String)
commentTailStar ys@[] = (ys,ys,ys)
commentTailStar ys@(x:xs) | x == '/'  = ([],[],xs)
                          | x == '*'  = commentTailStar xs
                          | otherwise = commentTail xs

operatorState:: String -> (String, String, String)
operatorState ys@[]  = ("operator",ys,ys)
operatorState ys@(x:xs) | isSingleOperator x = let (str,ws,zs) = operatorState xs in (str,x:ws,zs)
                        | otherwise    = ("operator",[],ys)

readStr:: String -> (String,String,String)
readStr ys@[] = ("string",ys,ys)
readStr ys@(x:xs) | x == '\"' = ("string",[x],xs)
                  | otherwise = let (str,ws,zs) = readStr xs in (str,x:ws,zs)

readChar:: String -> (String,String,String)
readChar ys@[] = error "\nInvalid character\n"
readChar ys@(x:xs) |x == '\\' = let (str,ws,zs) = verifyChar2 xs in (str,x:ws,zs)
                   |otherwise = let (str,ws,zs) = verifyChar xs in (str,x:ws,zs)

verifyChar:: String -> (String,String,String)
verifyChar ys@[] = error "\nInvalid character, you don't define the character\n"
verifyChar ys@(x:xs) | x == '\'' = ("character",[x],xs)
                     | otherwise = error "\nInvalid character, you don't define the character\n"

verifyChar2:: String -> (String,String,String)
verifyChar2 ys@[] = error "\nInvalid character, you don't define the character\n"
verifyChar2 ys@(x:xs) = let (str,ws,zs) = verifyChar xs in (str,x:ws,zs)

isWhitespace   x = elem x [' ','\n','\t','\r']
isDigit        x = elem x "0123456789"
isWord         x = elem x "abcdefghijklmnopqrstuvwxyz_ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"
isLetter       x = elem x "abcdefghijklmnopqrstuvwxyz_ABCDEFGHIJKLMNOPQRSTUVWXYZ"
isSeparator    x = elem x ".,;{}[]()"
isSingleOperator x = elem x "=><!~?:-&~%^*/+|"
isOperator     x = elem x ["=",  ">",  "<",  "!",  "~",  "?",  ":",  "->",
                           "==", ">=", "<=", "!=", "&&", "||", "++", "--",
                           "+",  "-",  "*",  "/",  "&",  "|",  "^",  "%",  "<<",  ">>",  ">>>",
                           "+=", "-=", "*=", "/=", "&=", "|=", "^=", "%=", "<<=", ">>=", ">>>="]
isReservedWord::String -> Bool
isReservedWord x = elem x ["abstract","assert","boolean","break","byte","case","catch","char","class","const",
                           "default","do","double","else","enum","extends","false","final","finally","float",
                           "gota","if","implements","import","instanceof","int","interface","long","native",
                           "new","null","package","private","protected","public","return","static","super",
                           "throw","throws","true","try","void","while","continue"]
