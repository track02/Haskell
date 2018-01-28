and' :: [Bool] -> Bool
and' [x] = x
and' (x:xs) = x && (and' xs)

concat' :: [[a]] -> [a]
concat'  [] = []
concat' (x:xs) = x ++ (concat' xs)

replicate' :: Int -> a -> [a]
replicate' 0 _ = []
replicate' n x  = x : (replicate' (n - 1) x)

nth :: [a] -> Int -> a
nth (x:xs) 0 = x
nth (x:xs) n = nth xs (n - 1)

elem' :: Eq a => a -> [a] -> Bool
elem' a [] = False
elem' a (x:xs) | a == x = True
               | otherwise = elem' a xs