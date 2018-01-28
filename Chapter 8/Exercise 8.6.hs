using folde define a function

data Expr = Val Int :: Add Expr Expr

eval :: Expr -> Int 

that calculates the number of values in a given expression