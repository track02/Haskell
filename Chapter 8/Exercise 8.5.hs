Given 

data Expr = Val Int | Add Expr Expr

define a higher order function

			--f           --g           
folde :: (Int -> a) -> (a -> a -> a) -> Expr -> a

such that folde replaces each Val constructor in an expression
by the function f and each add constructor by the function g 

