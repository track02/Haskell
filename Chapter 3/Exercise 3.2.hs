bools :: [Bool]
bools = [True, True]

nums :: [[Int]]
nums = [[1,2,3],[4,5,6]]

add :: Int -> Int -> Int -> Int
add x y z = x + y + z

copy :: a -> (a,a)
copy x = (x, y)

apply :: (a -> b) -> a -> b
apply f a = f a