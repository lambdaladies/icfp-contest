import Data.Set (Set, union, singleton, empty)
import Data.Bits

data UnOp = Not | Shl1 | Shr1 | Shr4 | Shr16 deriving (Show, Eq, Ord)
data BinOp = And | Or | Xor | Plus deriving (Show, Eq, Ord)
data Ops = UnaryOp UnOp | BinaryOp BinOp | IfZeroOp | TFoldOp | FoldOp deriving (Show, Eq, Ord)

data Variable = X | Y | Z deriving (Show, Eq, Ord)

data Expr  = Zero
           | One
           | Var Variable
           | FoldLambda Expr Expr Expr
           | IfZero Expr Expr Expr
           | Unary UnOp Expr
           | Binary BinOp Expr Expr
           deriving (Show, Eq, Ord)

size (               Zero) = 1
size (                One) = 1
size (              Var v) = 1
size (    IfZero e0 e1 e2) = 1 + size e0 + size e1 + size e2
size (FoldLambda e0 e1 e2) = 2 + size e0 + size e1 + size e2
size (       Unary op1 e0) = 1 + size e0
size (   Binary op2 e0 e1) = 1 + size e0 + size e1

program_size e0 = 1 + size e0

opset (               Zero) = empty
opset (                One) = empty
opset (              Var v) = empty
opset (    IfZero e0 e1 e2) = (singleton IfZeroOp) `union` (opset  e0) `union` (opset  e1) `union` (opset  e2)
opset (FoldLambda e0 e1 e2) = (singleton FoldOp) `union` (opset e0) `union` (opset e1) `union` (opset e2)
opset (       Unary op1 e0) = (singleton $ UnaryOp op1) `union` (opset  e0)
opset (   Binary op2 e0 e1) = (singleton $ BinaryOp op2) `union` (opset  e0) `union` (opset  e1)

operators (FoldLambda (Var X) Zero e) = (singleton TFoldOp) `union` (opset e)
operators e                           = opset e

-- Whole programs
interp :: Expr -> Integer -> Integer
-- Intermediate expressions, evaluated in a 3-var environment
eval :: Expr -> (Maybe Integer, Maybe Integer, Maybe Integer) -> Integer

interp e i = eval e (Just i, Nothing, Nothing)

eval Zero _ = 0x0000000000000000
eval One _ = 0x0000000000000001
eval (Var X) (Just i, _, _) = i
eval (Var Y) (_, Just i, _) = i
eval (Var Z) (_, _, Just i) = i
eval (IfZero e1 e2 e3) env = if (eval e1 env) == 0 then eval e2 env else eval e3 env 
eval (Unary o e) env = eval_unary o (eval e env)
eval (Binary o e1 e2) env = eval_binary o (eval e1 env) (eval e2 env)

eval_unary Not i = complement i
eval_unary Shl1 i = shift i 1
eval_unary Shr1 i = shift i (-1)
eval_unary Shr4 i = shift i (-4)
eval_unary Shr16 i = shift i (-16)

-- why aren't these in scope?
-- eval_binary And x y = x & y
-- eval_binary Or x y = x|y
eval_binary Xor x y = xor x y
eval_binary Plus x y = x + y
