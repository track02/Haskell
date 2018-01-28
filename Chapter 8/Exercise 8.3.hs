Conside the tree type

data Tree a = Leaf a | Node (Tree a) a (Tree a)

a tree is balanced if the no. leaves in the left/right subtree
differ by 1 at the most, define the following functions the checks if a given tree is balanced

balanced :: Tree a -> Bool 

firstly define a function which returns the no. of leaves in a tree 