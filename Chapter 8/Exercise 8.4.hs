-- Define the following function that converts a non-empty list into a balanced tree

-- Firstly define a function that splits a list into two halves whose length differs by at least one 

data Tree a = Leaf a | Node (Tree a) a (Tree a)

list_splitter :: [a] -> ([a],[a])
list_splitter xs = (take mid xs, drop mid xs)
                 where mid = quot (length xs) 2

-- Given a list [1,2,3,4,5]
-- Split into two halves [1,2] , [3,4,5]
-- take first value of 2nd list to use as node value and drop node 
-- left node becomes balance first list
-- right node becomes balance 2nd list 
-- If list contains a single element return a leaf 

--  balance [1,2,3,4,5]
-- left = [1,2]
-- node_val = 3
-- right = [4,5]

-- balance [1,2]                           -- balance [4,5]
-- left = [1]                              -- left = [4]
-- node_val = 2                            -- node_val = 5
-- right = []                              -- right = [ ]

-- balance [1]                             -- balance [4]
-- LEAF 1                                  -- LEAF 4

-- balance []                              -- balance []
-- ???                                     -- ???
--
--                            [3]
--                           /   \				 
--                          /     \
--                        [2]     [5]                        
--                       /  \    /   \
--                     (1)   ? (4)    ?

balance :: [a] -> Tree a 
--balance []     = Leaf 0
balance (x:[]) = Leaf x 
balance xs     = Node (balance left) node_val (balance right)
               where 
			   split = list_splitter xs
			   left = fst split
			   node_val = (snd split) !! 0 -- Take first element 
			   right = drop 1 (snd split)