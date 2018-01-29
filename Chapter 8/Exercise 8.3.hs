-- Conside the tree type
-- a tree is balanced if the no. leaves in the left/right subtree differ by 1 at the most.
-- define the following function that checks if a given tree is balanced
-- firstly define a function which returns the no. of leaves in a tree 

data Tree a = Leaf a | Node (Tree a) a (Tree a)

leaves :: Tree a -> Integer
leaves (Leaf _)     = 1 -- Evaluate to 1 when a leaf is given as an argument
leaves (Node l v r) = leaves l + leaves r -- Otherwise visit each node and sum up the no. leaves 

balanced :: Tree a -> Bool -- To find if a tree is balanced 
balanced (Leaf _) = True -- Tree of a single leaf is balanced  
balanced (Node l v r) = (diff >= -1) && (diff <= 1)  -- Is difference between l/r in range -1 .. 1 
                      where diff = (leaves l - leaves r) -- Split tree in half and find difference in leaves on each side 


