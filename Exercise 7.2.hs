all' :: (a -> Bool) -> [a] -> Bool
all' p xs = and (map p xs)

any' :: (a -> Bool) -> [a] -> Bool
any' p xs = or (map p xs)

-- Take While, keep taking elements whilst the current element is true
-- If current element is false then stop
takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' _ [] = []
takeWhile' p (x:xs) | p x = x : takeWhile' p xs
                    | otherwise = []


dropWhile' :: (a -> Bool) -> [a] -> [a]
dropWhile' _ [] = []
dropWhile' p (x:xs) | p x = dropWhile' p xs
                    | otherwise = (x:xs)