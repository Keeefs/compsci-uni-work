module Expr where
 
data Expr
    = Num Int
    | Var String
    | Add Expr Expr
    | Mul Expr Expr
    deriving (Show, Eq)
 
-- Environment definition for variable lookups
type Env = [(String, Int)]
 
-- Function Stubs: Implement using pattern matching and recursion
eval :: Env -> Expr -> Int
eval _ (Num value) = value

eval env (Var variable) = 
    case lookup variable env of
        Nothing    -> error ("Unbound variable: " ++ variable)
        Just value -> value

eval env (Add left right) = (eval env left) + (eval env right) 

eval env (Mul left right) = (eval env left) * (eval env right) 

toString :: Expr -> String
toString (Num value) = show value 

toString (Var variable) = variable

toString (Add left right) = concat ["(", (toString left), " + ", (toString right), ")"]

toString (Mul left right) = concat ["(", (toString left), " * ", (toString right), ")"]


simplify :: Expr -> Expr
simplify = error "TODO: Implement simplify"
