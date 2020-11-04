module MyScanner where

data Symbol = ReservedWord   String
            | Separator      String
            | Operator       String
            | Identifier     String
            | IntegerLiteral String
            | BooleanLiteral    String
        deriving (Show)

instance Eq Symbol where
   (Identifier i1) == (Identifier i2) = True
   (ReservedWord p1) == (ReservedWord p2) = p1 == p2
   (Separator s1) == (Separator s2) =  s1 == s2
   (Operator o1) == (Operator o2) = o1 == o2
   (IntegerLiteral i1) == (IntegerLiteral i2) = True
   (BooleanLiteral b1) == (BooleanLiteral b2) = True
   _ == _ = False
         
trozar:: String -> [Symbol]
trozar [] = []
trozar xs = 
             case rs of 
                "digit"     -> (IntegerLiteral ws):trozar zs
                "reserved"  -> (ReservedWord ws):trozar zs
                "boolean"   -> (BooleanLiteral ws):trozar zs
                "word"      -> (Identifier ws):trozar zs
                "operator"  -> (Operator ws):trozar zs
                "separator" -> (Separator ws):trozar zs
                _ -> trozar zs
             where
                 (rs, ws, zs) = sourceState xs
                 
sourceState:: String -> (String, String, String)
sourceState ys@[] = (ys,ys,ys)
sourceState (x:xs) | isWhitespace x = sourceState xs
                   | isDigit x      = let (str,ws,zs) = digitState xs     in (str,x:ws,zs)
                   | isWord  x      = let (str,ws,zs) = wordState xs     
                            in if isReservedWord (x:ws) then ("reserved",x:ws,zs)
                               else if isBooleanLiteral (x:ws) then ("boolean", x:ws,zs) else (str,x:ws,zs)
                   | isSeparator x  = ("separator",[x],xs)
                   | x == '/'       = let (str,ws,zs) = slash xs
                              in if str == "operator" then (str,x:ws,zs)
                                 else (str,[],zs)
                   | isSingleOperator  x  = let (str,ws,zs) = operatorState xs
                            in  if isOperator (x:ws) then ("operator",x:ws,zs)
                                else error ("\nInvalid operator "++(x:ws))
                   | otherwise      = error "\nInvalid character\n"

digitState::String -> (String, String, String)
digitState ys@[] = ("digit",ys,ys)
digitState ys@(x:xs) | isLetter x = error "\nIs not a valid literal Number\n" 
                     | isDigit x  =  let (str,ws,zs) = digitState xs in (str,x:ws,zs)
                     | otherwise  = ("digit",[], ys)

wordState::String -> (String, String, String)
wordState ys@[] = ("word",ys,ys)
wordState ys@(x:xs) | isWord x  =  let (str,ws,zs) = wordState xs in (str,x:ws,zs)
                    | otherwise = ("word",[],ys)

slash::String -> (String, String, String)
slash ys@(x:xs) | x == '*'  = commentTail xs
                | x == '/'  = endOfLineComment xs
                | otherwise = ("operator",[],ys)

commentTail::String -> (String, String, String)
commentTail ys@(x:xs) | x == '*' = commentTailStar xs
                      | otherwise = commentTail xs
                             
commentTailStar::String -> (String, String, String)
commentTailStar ys@(x:xs) | x == '/' = ([],[],xs)
                          | x == '*' = commentTailStar xs
                          | otherwise = commentTail xs

endOfLineComment::String -> (String, String, String)
endOfLineComment ys@(x:xs) | x == '\n' = ("commentary",[], xs)
                           | otherwise = endOfLineComment xs
                         
operatorState:: String -> (String, String, String)
operatorState ys@[]  = ("operator",ys,ys)
operatorState ys@(x:xs) | isSingleOperator x = let (str,ws,zs) = operatorState xs in (str,x:ws,zs)
                        | otherwise    = ("operator",[],ys)
                        

isWhitespace   x = elem x [' ','\t','\f', '\n']
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
                           "default","do","double","else","enum","extends","final","finally","float",
                           "gota","if","implements","import","instanceof","int","interface","long","native",
                           "new","null","package","private","protected","public","return","static","super",
                           "throw","throws","try","void","while","continue"]
isBooleanLiteral x = elem x ["true", "false"]

