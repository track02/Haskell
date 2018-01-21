fac :: Int -> Int
fac 0 = 1
fac n | n > 0 = n * fac (n - 1)

-- If negative, infinite recursion as 0 never reached