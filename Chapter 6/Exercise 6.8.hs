merge :: Ord a => [a] -> [a] -> [a]
merge xs [] = xs 
merge [] ys = ys
-- Mpve smaller value to front and then repeat, removing smaller value from comparison pool
merge (x:xs) (y:ys) | x >= y = y : (merge (x:xs) ys)
                    | x < y =  x : (merge xs (y:ys))

msort :: Ord a => [a] -> [a]
msort [] = []
msort [a] = [a]
msort xs = merge (msort left) (msort right)
           where (left, right) = halve xs

halve :: [a] -> ([a],[a])
halve xs = (take mid xs, drop mid xs) 
           where mid = (length xs) `div` 2
