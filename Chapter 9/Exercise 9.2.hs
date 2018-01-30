-- Define a recursive function isChoice
-- that decides if one list is chosen from another
-- Without using perms and subs

-- Start by defining a function that removes the first occurrence of a value from a list

removeVal :: Eq a => a -> [a] -> [a]
removeVal _ []     = []
removeVal a (x:xs) | a == x = xs 
                   | otherwise = x : (removeVal a xs) 


-- Given a list, is that list one of the generated choices of a second list?
-- E.g. isChoice [1] [1,2]
-- choices [1,2] -> [[],[2],[1],[1,2],[2,1]] -> True

-- We traverse the first list and remove each of its elements from the second 
-- If we can reach the end of the first list whilst successfully removing each element 
-- then it must be a generated choice as all its values were members of the second list  

isChoice :: Eq a => [a] -> [a] -> Bool
isChoice [] ys = True 
isChoice (x:xs) ys | x `elem` ys = isChoice xs (removeVal x ys)
                   | otherwise = False