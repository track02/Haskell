-- Modify the final program to:

-- allow the use of exponentiation in expressions

-- produce the nearest solutions if no exact solution is possible

-- order the solutions using a suitable measure of simplicity

-- The countdown program works as follows:
-- 1) From a given list of numbers n and a target value t
-- 2) Determine all possible arrangements of the numbers E.g. [[1,2,3], [3,2,1], [1], [2], [2,1] ... ]
-- 3) From these possible arrangements determine all possible expressions E.g. [[2 + 1], [2 / 1], [2 * 1], [2 ^ 1] ... ]
-- 4) Now evaluate each of these expressions, removing any which do not result in the target value t 
-- 5) Return the final list of expressions as the possible solutions which evaluate to t 


import Data.List (sortBy, groupBy)
import Data.Ord (comparing)

data Op = Add | Sub | Mul | Div | Exp -- Add exponentiation operation x ^ y

instance Show Op where
   show Add = "+"
   show Sub = "-"
   show Mul = "*"
   show Div = "/"
   show Exp = "^" -- Display method for an Exp operation
   

valid :: Op -> Int -> Int -> Bool -- Rules to determine if a given operation is valid 
valid Add _ _ = True
valid Sub x y = x > y
valid Mul _ _ = True
valid Div x y = y /= 0 && x `mod` y == 0
valid Exp x y = x > 0 && y > 0 

apply :: Op -> Int -> Int -> Int -- How to apply each operation 
apply Add x y = x + y
apply Sub x y = x - y
apply Mul x y = x * y
apply Div x y = x `div` y
apply Exp x y = x ^ y 

-- Numeric expressions

data Expr = Val Int | App Op Expr Expr -- Representation of an expression

instance Show Expr where -- Method for outputting expressions 
   show (Val n)     = show n
   show (App o l r) = brak l ++ show o ++ brak r
                      where
                         brak (Val n) = show n
                         brak e       = "(" ++ show e ++ ")"

values :: Expr -> [Int]  
values (Val n)     = [n]
values (App _ l r) = values l ++ values r

eval :: Expr -> [Int] -- Evaluates an expression to a value 
eval (Val n)     = [n | n > 0]
eval (App o l r) = [apply o x y | x <- eval l,
                                  y <- eval r,
                                  valid o x y]

-- Combinatorial functions

subs :: [a] -> [[a]] -- Determines all subsets of a list 
subs []     = [[]]
subs (x:xs) = yss ++ map (x:) yss
              where yss = subs xs

interleave :: a -> [a] -> [[a]] -- Determines all possible interleave positions of a value x in a list xs 
interleave x []     = [[x]]
interleave x (y:ys) = (x:y:ys) : map (y:) (interleave x ys)

perms :: [a] -> [[a]] -- Determines all possible permutations of a list 
perms []     = [[]]
perms (x:xs) = concat (map (interleave x) (perms xs))

choices :: [a] -> [[a]] -- Full list of choices for a given list - combining the above three functions 
choices = concat . map perms . subs

-- Formalising the problem

solution :: Expr -> [Int] -> Int -> Bool
solution e ns n = elem (values e) (choices ns) && eval e == [n] 


-- Brute force solution

split :: [a] -> [([a],[a])]
split []     = []
split [_]    = []
split (x:xs) = ([x],xs) : [(x:ls,rs) | (ls,rs) <- split xs]

exprs :: [Int] -> [Expr]
exprs []  = []
exprs [n] = [Val n]
exprs ns  = [e | (ls,rs) <- split ns,
                 l       <- exprs ls,
                 r       <- exprs rs,
                 e       <- combine l r]

combine :: Expr -> Expr -> [Expr]
combine l r = [App o l r | o <- ops]

ops :: [Op]
ops = [Add,Sub,Mul,Div,Exp]

--            List   Target  Results  
solutions :: [Int] -> Int -> [Expr]
solutions ns n = [e | ns' <- choices ns, e <- exprs ns', eval e == [n]] 

-- We'll make a tolerant solutions function that accepts expressions that come close to n for a given tolerance 
tol_solutions :: [Int] -> Int -> Int -> [Expr]
tol_solutions ns n t = [e | ns' <- choices ns, e <- exprs ns', not (null (eval e)) && (abs((head (eval e)) - n) <= t)]
                                                                    --If result is not empty and difference between result and target is within tolerance range

-- *Main> solutions [1,2,3] 20
-- []
-- *Main> tol_solutions [1,2,3] 20 1
-- []
-- *Main> tol_solutions [1,2,3] 20 2
-- []
-- *Main> tol_solutions [1,2,3] 20 3
-- []
-- *Main> tol_solutions [1,2,3] 20 4
-- [2^(1+3),2^(3+1),(1+3)^2,(3+1)^2]
-- *Main> tol_solutions [1,2,3] 20 5
-- [2^(1+3),2^(3+1),(1+3)^2,(3+1)^2]
-- *Main> tol_solutions [1,2,3] 20 6
-- [2^(1+3),2^(3+1),(1+3)^2,(3+1)^2]
-- *Main> tol_solutions [1,2,3] 20 7
--  [(1+2)^3,2^(1+3),(2+1)^3,2^(3+1),(1+3)^2,3^(1+2),(3+1)^2,3^(2+1)]
--}
					   

-- Solutions ordering
-- Order solutions by number of operations in an expression
order_solutions :: [Expr] -> [Expr]
order_solutions xs = sortBy (comparing op_complexity) xs -- Import the sortBy function to sort expressions using the op_complexity function (below)
					 
-- Helper function to sum up no. operations in a given expression 
-- Lets invent some complexity rankings for the different types of expression
op_complexity :: Expr -> Int 
op_complexity (Val _) = 0
op_complexity (App Add l r) = 1 + op_complexity l + op_complexity r 
op_complexity (App Sub l r) = 2 + op_complexity l + op_complexity r 
op_complexity (App Mul l r) = 3 + op_complexity l + op_complexity r 
op_complexity (App Div l r) = 4 + op_complexity l + op_complexity r 
op_complexity (App Exp l r) = 5 + op_complexity l + op_complexity r 

test_exp_1 = App Exp (Val 2) (Val 3)
test_exp_2 = App Exp (App Add (Val 5) (Val 6)) (Val 2)
