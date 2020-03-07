module Scanner where
main:: IO()
main = do
       contents <- readFile "Prueba2.java"
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
                "digit" -> (IntegerLiteral ws):scanner zs
                "word"  -> (Identifier ws):scanner zs
                "operator" -> (Operator ws):scanner zs
                _ -> scanner zs
             where
                 (rs, ws, zs) = s xs
s:: String -> (String, String, String)
s ys@[] = (ys,ys,ys)
s (x:xs) | isWhitespace x = s xs
         | isDigit x      = let (ws,zs) = d xs in ("digit",x:ws,zs)
         | isWord  x      = let (ws,zs) = w xs in ("word" ,x:ws,zs)
--		 | x == '/'       = let (ws,zs) = w xs in 
         | otherwise      = let (ws,zs) = a xs in ("operator",x:ws,zs)

d::String -> (String, String)
d ys@[] = (ys,ys)
d ys@(x:xs) | isDigit x =  let (ws,zs) = d xs in (x:ws,zs)
            | otherwise = ([], ys)

w::String -> (String, String)
w ys@[] = (ys,ys)
w ys@(x:xs) | isWord x  =  let (ws,zs) = w xs in (x:ws,zs)
            | otherwise = ([],ys)

a:: String -> (String, String)
a ys@[] = (ys,ys)
a xs = ([], xs)

isWhitespace x = elem x [' ','\n','\t','\r']
isDigit x = elem x "0123456789"
isWord  x = elem x "abcdefghijklmnopqrstuvwxyz_ABCDEFGHIJKLMNOPQRSTUVWXYZ123456789"
isDelimiter x = elem x ".,;{}[]()*/+-=&|"