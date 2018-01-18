
mult :: Int -> Int -> Int -> Int
mult x y z = x*y*z

mult_lambda :: Int -> Int -> Int -> Int
mult_lambda \x -> (\y -> (\z -> x * y * z))