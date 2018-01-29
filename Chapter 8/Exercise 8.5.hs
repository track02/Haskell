-- Given the following data type 

data Expr = Val Int | Add Expr Expr

-- Define a higher order function, folde 
-- such that folde replaces each Val constructor in an expression
-- by the function f and each add constructor by the function g 
        
folde :: (Int -> a) -> (a -> a -> a) -> Expr -> a
folde f g (Add e1 e2) = g (folde f g e1) (folde f g e2) -- If an add expression, replace add with g and call folde on each inner expr
folde f g (Val i)     = f i  --If value expression call f on the value


