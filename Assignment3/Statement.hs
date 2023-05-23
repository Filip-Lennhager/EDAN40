module Statement(T, parse, toString, fromString, exec) where
import Prelude hiding (return, fail)
import Parser hiding (T)
import qualified Dictionary
import qualified Expr
import Text.Read (Lexeme(String))

type T = Statement
data Statement =
    Assignment String Expr.T |
    Skip |
    Begin [Statement] |
    If Expr.T Statement Statement |
    While Expr.T Statement |
    Read String |
    Write Expr.T
    deriving Show

-- Parsing functions
assignment :: Parser Statement
assignment = word #- accept ":=" # Expr.parse #- require ";" >-> buildAss
skip :: Parser Statement
skip = accept "skip" #- require ";" >-> \_ -> Skip
begin :: Parser Statement
begin = accept "begin" -# iter parse #- require "end" >-> Begin
ifSt :: Parser Statement
ifSt = accept "if" -# Expr.parse # require "then" -# parse # require "else" -# parse >-> buildIf
while :: Parser Statement
while = accept "while" -# Expr.parse #- require "do" # parse >-> buildWhile
readSt :: Parser Statement
readSt = accept "read" -# word #- require ";" >-> Read
write :: Parser Statement
write = accept "write" -# Expr.parse #- require ";" >-> Write

-- Builder functions
buildAss :: (String, Expr.T) -> Statement
buildAss (v, e) = Assignment v e
buildIf :: ((Expr.T, Statement), Statement) -> Statement
buildIf ((cond, thenStmt), elseStmt) = If cond thenStmt elseStmt
buildWhile :: (Expr.T, Statement) -> Statement
buildWhile (cond, stmt) = While cond stmt

indent :: Int -> [Char]
indent = flip take (repeat '\t')

-- Function format takes an indentation level and a Statement, and returns a formatted string.
format :: Int -> Statement -> String
format ind (Assignment v e)   = indent ind ++ v ++ " := " ++ Expr.toString e ++ ";\n"
format ind (Skip)             = indent ind ++ "skip;\n"
format ind (Begin stmts)      = indent ind ++ "begin\n" ++ concatMap (format (ind+1)) stmts ++ indent ind ++ "end\n"
format ind (If cond thenStmt elseStmt) = indent ind ++ "if " ++ Expr.toString cond ++ " then\n" ++ format (ind+1) thenStmt ++ indent ind ++ "else\n" ++ format (ind+1) elseStmt
format ind (While cond stmt)  = indent ind ++ "while " ++ Expr.toString cond ++ " do\n" ++ format (ind+1) stmt
format ind (Read v)           = indent ind ++ "read " ++ v ++ ";\n"
format ind (Write e)          = indent ind ++ "write " ++ Expr.toString e ++ ";\n"

exec :: [T] -> Dictionary.T String Integer -> [Integer] -> [Integer]
exec [] _ _ = []
exec (Assignment v e : stmts) dict input = exec stmts (Dictionary.insert (v, Expr.value e dict) dict) input
exec (Skip : stmts) dict input = exec stmts dict input
exec (Begin bStmts : stmts) dict input = exec (bStmts ++ stmts) dict input
exec (If cond thenStmts elseStmts: stmts) dict input =
    if (Expr.value cond dict) > 0
    then exec (thenStmts: stmts) dict input
    else exec (elseStmts: stmts) dict input
exec ((While cond stmt) : stmts) dict input =
    if (Expr.value cond dict) > 0
    then exec (stmt : (While cond stmt) : stmts) dict input
    else exec stmts dict input
exec (Read v : stmts) dict (i:input) = exec stmts (Dictionary.insert (v, i) dict) input
exec (Write e : stmts) dict input = Expr.value e dict : exec stmts dict input

-- Removes comments from program string
stripComments :: String -> String
stripComments [] = []
stripComments ('-': '-': xs) = strip xs
    where
        strip :: String -> String
        strip [] = []
        strip ('\n': xs) = stripComments xs
        strip (_:xs) = strip xs
stripComments (x:xs) = x : stripComments xs

instance Parse Statement where
  parse :: Parser Statement
  parse = (assignment ! skip ! begin ! ifSt ! while ! readSt ! write) . stripComments
  toString :: Statement -> String
  toString = format 0
