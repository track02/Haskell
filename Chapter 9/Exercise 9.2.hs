-- Define a recursive function isChoice
-- that decides if one list is chosen from another
-- Without using perms and subs

-- Start by defining a function that removes the first occurrence of a value from a list

isChoice :: Eq a => [a] -> [a] -> Bool