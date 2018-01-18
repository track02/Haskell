grid :: Int -> Int -> [(Int,Int)]
grid x y = [(x', y') | x' <- [0..x], y' <- [0..y]]
-- Remember, successive generator can be viewed as being nested for 0 to x -> for 0 to y