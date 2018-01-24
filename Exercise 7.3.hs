foldmap :: (a -> b) -> [a] -> [b]
foldmap f = foldr (\x xs -> f x : xs) [] 
-- apply the function to first list element then cons with second


foldfilter :: (a -> Bool) -> [a] -> [a]
foldfilter p = foldr (\x xs -> if p x then x : xs else xs) []
-- Evaluate the two list elements, drop x if it doesnt satisfy predicate