merge :: Ord a => [a] -> [a] -> [a]
merge xs [] = xs 
merge [] ys = ys
-- Mpve smaller value to front and then repeat, removing smaller value from comparison pool
merge (x:xs) (y:ys) | x >= y = y : (merge (x:xs) ys)
                    | x < y =  x : (merge xs (y:ys))