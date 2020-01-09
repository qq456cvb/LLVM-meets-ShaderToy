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
                                else Kwd c:lexer stream
                    
data Expr =
      ExprNum Float
    | ExprVar String
    | ExprBin Char Expr Expr
    | ExprCall String [Expr] deriving (Show)


data Proto = Protortpe String [String]
data Func = Function Proto Expr

parse_primary :: [Token] -> Expr
parse_primary content = 
    case content of [] -> error "empty token"
                    [Number n] -> ExprNum n
                    [Kwd '(':e:Kwd ')'] -> e
                    Identy id:stream -> parse_ident id stream
                    _ -> error "unknown token"
    where e = parse_expr t

parse_expr :: [Token] -> Expr
parse_expr content = 
    case content of prim:stream -> parse_bin 0 lhs stream
    where lhs = parse_primary prim

parse_bin :: Int -> Expr -> [Token] -> Expr
parse_bin prec lhs content =
    case content of Kwd c:stream -> if member c precedence then
                                        if precedence ! c < precedence ! prec then lhs 
                                        else let rhs = parse_primary stream
                                                in if 
                                    else lhs
                    _ -> lhs
    

main = do 
    let tokens = lexer "#comment \n hel77lo67 def 3456 yy "
        -- in print $ parse_primary tokens
        in print $ precedence ! '+' < precedence ! '/'