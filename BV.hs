module BV where

import Data.Function (fix)
import Data.Maybe (catMaybes)
import qualified Data.Map as Map
import Data.Set (Set, union, singleton, empty, member, fromList)
import Data.Bits (Bits, (.|.), (.&.), xor, complement, shiftL, shiftR)
import Data.Word (Word64)

type Vector = Word64

data UnOp = Not | Shl1 | Shr1 | Shr4 | Shr16 deriving (Eq, Ord)
data BinOp = And | Or | Xor | Plus deriving (Eq, Ord)
data TernOp = IfZero deriving (Eq, Ord)
data FoldOp = Fold | TFold deriving (Eq, Ord)

data Ops = UnaryOp UnOp | BinaryOp BinOp | TernaryOp TernOp | TFoldOp | FoldOp deriving (Show, Eq, Ord)

data Variable = X | Y | Z deriving (Show, Eq, Ord)

type ExprSize = Int

data Expr  = Zero
           | One
           | Var Variable
           | FoldLambdaYZ Expr Expr Expr
           | Unary UnOp Expr
           | Binary BinOp Expr Expr
           | Ternary TernOp Expr Expr Expr
           deriving (Eq, Ord)

data Operators = Operators {
    unary   :: [UnOp],
    binary  :: [BinOp],
    ternary :: [TernOp],
    folds   :: [FoldOp]
} deriving (Show, Eq, Ord)


instance Show UnOp where
    show Not   = "not"
    show Shl1  = "shl1"
    show Shr1  = "shr1"
    show Shr4  = "shr4"
    show Shr16 = "shr16"

instance Show BinOp where
    show And   = "and"
    show Or    = "or"
    show Xor   = "xor"
    show Plus  = "plus"

instance Show TernOp where
    show IfZero = "if0"

instance Show FoldOp where
    show Fold   = "fold"
    show TFold  = "tfold"

parse_unary   "not"   = Just Not
parse_unary   "shl1"  = Just Shl1
parse_unary   "shr1"  = Just Shr1
parse_unary   "shr4"  = Just Shr4
parse_unary   "shr16" = Just Shr16
parse_unary   _       = Nothing

parse_binary  "and"   = Just And
parse_binary  "or"    = Just Or
parse_binary  "xor"   = Just Xor
parse_binary  "plus"  = Just Plus
parse_binary  _       = Nothing

parse_ternary "if0"   = Just IfZero
parse_ternary _       = Nothing

parse_folds   "fold"  = Just Fold
parse_folds   "tfold" = Just TFold
parse_folds   _       = Nothing

parse_opstrings opstrings = Operators {
    unary   = catMaybes $ map parse_unary opstrings,
    binary  = catMaybes $ map parse_binary opstrings,
    ternary = catMaybes $ map parse_ternary opstrings,
    folds   = catMaybes $ map parse_folds opstrings
}

instance Show Expr where
    show (                 Zero) = "0"
    show (                  One) = "1"
    show (                Var X) = "x"
    show (                Var Y) = "y"
    show (                Var Z) = "z"
    show (FoldLambdaYZ e0 e1 e2) = "(fold " ++ show e0 ++ " " ++ show e1 ++ " (lambda (y z) " ++ show e2 ++ "))"
    show (         Unary op1 e0) = "(" ++ show op1 ++ " " ++ show e0 ++ ")"
    show (     Binary op2 e0 e1) = "(" ++ show op2 ++ " " ++ show e0 ++ " " ++ show e1 ++ ")"
    show ( Ternary op3 e0 e1 e2) = "(" ++ show op3 ++ " " ++ show e0 ++ " " ++ show e1 ++ " " ++ show e2 ++ ")"

size (                 Zero) = 1
size (                  One) = 1
size (                Var v) = 1
size (FoldLambdaYZ e0 e1 e2) = 2 + size e0 + size e1 + size e2
size (         Unary op1 e0) = 1 + size e0
size (     Binary op2 e0 e1) = 1 + size e0 + size e1
size ( Ternary op3 e0 e1 e2) = 1 + size e0 + size e1 + size e2

opset (                 Zero) = empty
opset (                  One) = empty
opset (                Var v) = empty
opset (FoldLambdaYZ e0 e1 e2) = (singleton FoldOp) `union` (opset e0) `union` (opset e1) `union` (opset e2)
opset (         Unary op1 e0) = (singleton $ UnaryOp op1) `union` (opset  e0)
opset (     Binary op2 e0 e1) = (singleton $ BinaryOp op2) `union` (opset  e0) `union` (opset  e1)
opset ( Ternary op3 e0 e1 e2) = (singleton $ TernaryOp op3) `union` (opset  e0) `union` (opset  e1) `union` (opset  e2)

operators (FoldLambdaYZ (Var X) Zero e) = (singleton TFoldOp) `union` (opset e)
operators e                             = opset e

-- Whole programs
interp :: Expr -> Vector -> Vector
-- Intermediate expressions, evaluated in a 3-var environment
eval :: Expr -> (Maybe Vector, Maybe Vector, Maybe Vector) -> Vector

interp e i = eval e (Just i, Nothing, Nothing)

eval (                 Zero) env = 0x0000000000000000
eval (                  One) env = 0x0000000000000001
eval (                Var X) (Just x, _, _) = x
eval (                Var Y) (_, Just y, _) = y
eval (                Var Z) (_, _, Just z) = z
eval (         Unary op1 e0) env = eval_unary op1 (eval e0 env)
eval (     Binary op2 e0 e1) env = eval_binary op2 (eval e0 env) (eval e1 env)
eval ( Ternary op3 e0 e1 e2) env = if (eval e0 env) == 0 then (eval e1 env) else (eval e2 env) 
eval (FoldLambdaYZ e0 e1 e2) env@(Just x, Nothing, Nothing) = foldr f (eval e1 env) (bytes $ eval e0 env)
    where f y z = eval e2 (Just x, Just y, Just z)

-- convert a 64-bit word to a list of 8 bytes-as-words in bigendian order
bytes i = [(shiftR i offset) .&. 0x00000000000000FF | offset <- [56,48..0]]


eval_unary   Not i = complement i
eval_unary  Shl1 i = shiftL i 1
eval_unary  Shr1 i = shiftR i 1
eval_unary  Shr4 i = shiftR i 4
eval_unary Shr16 i = shiftR i 16

eval_binary  And x y = x .&. y
eval_binary   Or x y = x .|. y
eval_binary  Xor x y = xor x y
eval_binary Plus x y = x + y

eval_ternary IfZero e x y = if e == 0 then x else y

unary_ops = [Not, Shl1, Shr1, Shr4, Shr16]
binary_ops = [And, Or, Xor, Plus]
ternary_ops = [IfZero]
fold_ops = [Fold, TFold]
supported_ops = Operators { unary=unary_ops, binary=binary_ops, ternary=ternary_ops, folds=[] }

triples :: ExprSize -> [(ExprSize, ExprSize, ExprSize)]
triples x = [(i, j, k) | i <- [1..(x-2)], (j, k) <- pairs (x-i)]
 where
  pairs y = [(i, y-i) | i <- [1..(y-1)]]

-- since all binary operators of BV are symmetric, we only need to consider applications
-- of binary operators where the second argument is no smaller than the first.
pairs_in_order :: ExprSize -> [(ExprSize, ExprSize)]
pairs_in_order x = [(i, x-i) | i <- [1..(x `div` 2)]]

-- eliminate some expressions that are always redundant
trivial = fromList [Unary Shr1 One, Unary Shr4 One, Unary Shr16 One]
nontrivial e = not $ member e trivial


generate_open recgen ops n
    | n <= 0    = []
    | n == 1    = [Zero, One, Var X]
    | n == 2    = filter nontrivial [Unary op1 e0 | op1 <- unary ops,
                                                    e0 <- recgen ops (n-1)]
    | otherwise = [Unary op1 e0         | op1 <- unary ops,
                                          e0 <- recgen ops (n-1)] ++
                  [Binary op2 e0 e1     | op2 <- binary ops,
                                          (i, j) <- pairs_in_order (n-1),
                                          e0 <- recgen ops i,
                                          e1 <- recgen ops j] ++
                  [Ternary op3 e0 e1 e2 | op3 <- ternary ops,
                                          (i, j, k) <- triples (n-1),
                                          e0 <- recgen ops i,
                                          e1 <- recgen ops j,
                                          e2 <- recgen ops k]

integerLength xs = toInteger $ length xs

l_generate_open l_recgen ops n
    | n <= 2    = integerLength $ generate ops n
    | otherwise = integerLength (unary ops)   * l_recgen ops (n-1) +
                  integerLength (binary ops)  * sum [l_recgen ops i * l_recgen ops j
                                                    | (i, j) <- pairs_in_order (n-1)] +
                  integerLength (ternary ops) * sum [l_recgen ops i * l_recgen ops j * l_recgen ops k
                                                    | (i, j, k) <- triples (n-1)]


show_program e0 = "(lambda (x) " ++ show e0 ++ ")"
size_program e0 = 1 + size e0
eval_program e0 i = eval e0 (Just i, Nothing, Nothing)

generate :: Operators -> Int -> [Expr]
generate ops n = memoize2 generate_open ops n
--generate = fix generate_open

l_generate :: Operators -> Int -> Integer
l_generate ops n = memoize2 l_generate_open ops n
--l_generate = fix l_generate_open

memoize2 f ops n = just_lookup (ops, n) $ loop f Map.empty ops 0 n

just_lookup k m = result
    where Just result = Map.lookup k m

loop f memo ops i n
    | i < n     = loop f memo' ops (i+1) n
    | otherwise = memo'
    where memo' = Map.insert (ops, i) (f recf ops i) memo
          recf ops' i' = just_lookup (ops', i') memo
