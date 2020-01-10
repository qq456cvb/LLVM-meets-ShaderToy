import Control.Monad  
import Data.Char
import Data.Typeable
import Data.Map

data Token =  
      Def
    | Extern
    | Identy String
    | Number Float
    | Kwd Char deriving (Show)

precedence = fromList [('<', 10), ('>', 10), ('+', 20), ('-', 20), ('*', 30)]

lexer_ident :: String -> String -> [Token]
lexer_ident buffer content = 
    case content of [] -> [id]
                    c:stream -> if isAlphaNum c then lexer_ident (buffer ++ [c]) stream
                                else id:lexer stream
    where id =  if buffer == "def" then Def
                else if buffer == "extern" then Extern
                else Identy buffer

lexer_num :: String -> String -> [Token]
lexer_num buffer content = 
    case content of [] -> [Number $ read buffer]
                    c:stream -> if (isNumber c || c == '.')then lexer_num (buffer ++ [c]) stream
                                else (Number $ read buffer):lexer stream

lexer_comment :: String -> [Token]
lexer_comment content = 
    case content of [] -> []
                    c:stream -> if c == '\n' then lexer stream
                                else lexer_comment stream

lexer :: String -> [Token]
lexer content = 
    case content of [] -> []
                    c:stream -> if isAlpha c then lexer_ident [c] stream
                                else if isNumber c then lexer_num [c] stream
                                else if c == '#' then lexer_comment stream
                                else if c /= ' ' then Kwd c:lexer stream
                                    else lexer stream
                    
data Expr =
      ExprNum Float
    | ExprVar String
    | ExprBin Char Expr Expr
    | ExprCall String [Expr] deriving (Show)


data Proto = Protortpe String [String]
data Func = Function Proto Expr


-- parse_top :: [Token] -> [Expr]
-- parse_top content =
--     case content of [] -> []
--                     Kwd ';':stream -> parse_top stream
--                     _ -> parse_primary content

-- parse_primary :: Int -> [Token] -> Expr
-- parse_primary stack content = 
--     case content of [] -> if stack == 0 then lhs else error "unexpected closure"
--                     Number n:stream -> parse_bin 0 ExprNum n stream
--                     Kwd '(':stream -> parse_primary (stack + 1) stream
--                     -- Identy id:stream -> parse_ident id stream
--                     _ -> error "unknown token"
                    

-- parse_expr :: [Token] -> Expr
-- parse_expr content = 
--     case content of prim:stream -> parse_bin 0 lhs stream
--                                     where lhs = parse_primary prim

-- parse_bin :: Int -> Int -> Expr -> [Token] -> Expr
-- parse_bin stack prec lhs content =
--     case content of Kwd ')':stream -> if stack < 1 then error "unexpected closure"
--                                         else parse_bin (stack - 1) lhs stream
--                     Kwd c:stream -> if member c precedence then
--                                         if precedence ! c < prec then lhs
--                                         else let rhs = parse_primary stack stream in
--                                                 parse_bin stack prec ExprBin c lhs rhs stream
--                                     else lhs
--                     [] -> lhs
--                     _ -> 
    

main = do 
    let tokens = lexer "#comment \n hel77lo67 def 3456.6 yy "
        in print tokens
        -- in print $ precedence ! '+' < precedence ! '/'