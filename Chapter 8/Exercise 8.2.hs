-- The standard prelude defines the type Ordering and the function compare

--    data Ordering LT | EQ | GT

--    compare :: Ord a => a -> a -> Ordering

-- that decides if one value in an ordered type
-- is LT, EQ, GT than another ordered type value

-- Use this function to redefine occurs for search trees
-- Occurs determines if a given value is present in a given tree

-- Explain why this new definition is more efficient than the old occurs?
-- I believe this is more efficient as only a single comparison occurs for each occurs 
-- 
data Tree a = Leaf a | Node (Tree a) a (Tree a)


new_occurs :: Ord a => a -> Tree a -> Bool 
new_occurs x (Leaf y) = x == y
new_occurs x (Node l v r) | comp == EQ = True 
                          | comp == LT = new_occurs x l
                          | otherwise = new_occurs x r
                          where comp = compare x v


old_occurs :: Ord a => a -> Tree a -> Bool
old_occurs x (Leaf y) = x == y
old_occurs x (Node l y r) | x == y = True
                         | x < y = old_occurs x l
                         | otherwise = old_occurs x r
