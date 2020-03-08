module Scanner where
main:: IO()
main = do
       contents <- readFile "Prueba.java"
       let lista = scanner contents
       putStr (show lista)

data Symbol = ReservedWord   String
            | Separator      Char
            | Operator       String
            | Identifier     String
            | IntegerLiteral String
            | StringLiteral  String
            | CharLiteral    Char
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
                _ -> scanner zs
             where
                 (rs, ws, zs) = s xs
s:: String -> (String, String, String)
s ys@[] = (ys,ys,ys)
s (x:xs) | isWhitespace x = s xs
         | isDigit x      = let (str,ws,zs) = d xs     in (str,x:ws,zs)
         | isWord  x      = let (str,ws,zs) = w xs     
                            in if isReservedWord (x:ws) then ("reserved",x:ws,zs)
                               else (str,x:ws,zs)
         | isSeparator x  = ("separator",[x],xs)
         | x == '/'       = let (str,ws,zs) = slash xs
                              in if str == "operator" then (str,x:ws,zs)
                                 else (str,[],zs)
         | isOperator  x  = let (str,ws,zs) = a xs  in (str,x:ws,zs)
         | x == '\"'      = let (str,ws,zs) = readStr xs in (str,x:ws,zs)
         | otherwise      = error "\nInvalid character\n"

d::String -> (String, String, String)
d ys@[] = ("digit",ys,ys)
d ys@(x:xs) 
            | isLetter x = error "\nIs not a valid identifier\n" 
            | isDigit x  =  let (str,ws,zs) = d xs in (str,x:ws,zs)
            | otherwise  = ("digit",[], ys)

w::String -> (String, String, String)
w ys@[] = ("word",ys,ys)
w ys@(x:xs) | isWord x  =  let (str,ws,zs) = w xs in (str,x:ws,zs)
            | otherwise = ("word",[],ys)

slash::String -> (String, String, String)
slash ys@(x:xs) | x == '/'  = lineCommentary xs
                | x == '*'  = largeCommentary xs
                | otherwise = ("operator",[],ys)

lineCommentary::String -> (String, String, String)
lineCommentary ys@[] = ("commentary",ys,ys)
lineCommentary ys@(x:xs) | x == '\n' = ("commentary",[],xs)
                         | otherwise = lineCommentary xs

largeCommentary::String -> (String, String, String)
largeCommentary ys@[] = (ys,ys,ys)
largeCommentary ys@(x:xs) | x == '*' = star xs
                          | otherwise = largeCommentary xs

star::String -> (String, String, String)
star ys@[] = (ys,ys,ys)
star ys@(x:xs) | x == '/' = ([],[],xs)
               | x == '*' = star xs
               | otherwise = largeCommentary xs

a:: String -> (String, String, String)
a ys@[]  = ("operator",ys,ys)
a ys@(x:xs) | isOperator x = ("operator",[x],xs)
         | otherwise    = ("operator",[],ys)

readStr:: String -> (String,String,String)
readStr ys@[] = ("string",ys,ys)
readStr ys@(x:xs) | x == '\"' = ("string",[x],xs)
                  | otherwise = let (str,ws,zs) = readStr xs in (str,x:ws,zs)

isWhitespace   x = elem x [' ','\n','\t','\r']
isDigit        x = elem x "0123456789"
isWord         x = elem x "abcdefghijklmnopqrstuvwxyz_ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"
isLetter       x = elem x "abcdefghijklmnopqrstuvwxyz_ABCDEFGHIJKLMNOPQRSTUVWXYZ"
isSeparator    x = elem x ".,;{}[]()"
isOperator     x = elem x "*/+-=&|<>"
isReservedWord::String -> Bool
isReservedWord x = elem x ["abstract","assert","boolean","break","byte","case","catch","char","class","const",
                           "default","do","double","else","enum","extends","false","final","finally","float",
                           "gota","if","implements","import","instanceof","int","interface","long","native",
                           "new","null","package","private","protected","public","return","static","super",
                           "throw","throws","true","try","void","while","continue"]