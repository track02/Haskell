grid :: Int -> Int -> [(Int,Int)]
grid x y = [(x', y') | x' <- [0..x], y' <- [0..y]]
-- Remember, successive generator can be viewed as being nested for 0 to x -> for 0 to y

square :: Int -> [(Int, Int)]
square l =  [(x', y') | (x', y') <- grid l l, x' /= y']
-- Pairs are drawn from grid, guard against matching pairs to remove diagonal