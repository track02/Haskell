luhnDouble :: Int -> Int
luhnDouble x = if xdouble > 9 then xdouble - 9 else xdouble
               where xdouble = x * 2


luhn :: Int -> Int -> Int -> Int -> Bool
luhn a b c d = if luhnsum `mod` 10 == 0 then True else False
               where luhnsum = luhnDouble a 
                             + b
                             + luhnDouble c
                             + d