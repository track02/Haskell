-- Using folde define a function that calculates the number of values in a given expression

data Expr = Val Int | Add Expr Expr

folde :: (Int -> a) -> (a -> a -> a) -> Expr -> a
folde f g (Add e1 e2) = g (folde f g e1) (folde f g e2)
folde f g (Val i)     = f i 


eval :: Expr -> Int 
-- For every add expression replace it with (+)
-- For every val expression, discard the value and return 1
eval e = folde (\x -> 1) (+) e 
