The standard prelude defines the type 

data Ordering LT | EQ | GT

and the function

compare :: Ord a => a -> a -> Ordering

that decides if one value in an ordered type is LT, EQ, GT than another ordered type value

Use this function, redefine occurs for search trees

occurs :: Ord a => a -> Tree a -> Bool 

Why is this new definition more efficient?

data Tree a = Leaf a | Node (Tree a) a (Tree a)

oldoccurs :: Ord a => a -> Tree a -> Bool
occurs x (Leaf y) = x == y
occurs x (Node l y r) | x == y = True
                      | x < y = occurs x l
                      | otherwise = occurs x r
