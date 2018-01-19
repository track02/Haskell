-- Find returns a list of all values associated with a given key
-- in a list of key, value pairs
find :: Eq a => a -> [(a,b)] -> [b]
find k t = [v | (k', v) <- t, k == k']

-- positions returns the list of all positions
-- as which a value appears in a given list
-- E.g. positions 3 [3,4,5,6,3,7,8,3] -> [0,4,7]
-- Need to transform xs into pairs for find to search
-- [(3,0), (4,1) ... ]
-- x = value to find positions for
-- xs = list of values
positions :: Eq a => a -> [a] -> [Int]
positions x xs = (find x (zip xs [0..]))