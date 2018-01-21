(^) :: Int -> Int -> Int
n ^ 0 = 1
n ^  p = n * (n ^ (p - 1))