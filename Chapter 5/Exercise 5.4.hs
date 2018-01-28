my_replicate :: Int -> a -> [a]
my_replicate n a = [a | _ <- [1..n]]
-- Loop from 1 to n and add an a each time to list
-- Not concerned with result from generator