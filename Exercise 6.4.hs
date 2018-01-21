euclid :: Int -> Int -> Int
euclid a b | a == b = a
           | a < b = euclid a (b - a)
           | otherwise = euclid (a - b) b