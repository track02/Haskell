-- What effect would generalising the function split
-- to also return pairs containing the empty list have on the behaviour of solutions

-- Allowing split to return pairs with an empty list will adversely effect the solution 
-- E.g.
-- split [1,2] -> [([1,2],[])]

-- Testing this results in a (seemingly) infinite loop during expression generation 

-- split is part of the brute force solution and has the following implementation

split :: [a] -> [([a],[a])]
split []     = [] -- Removing these cases
split [_]    = []
split (x:xs) = ([x],xs) : [(x:ls,rs) | (ls,rs) <- split xs]

-- Split returns the possible split pairings of a list 
-- split [1,2,3]     -> [([1],[2,3]), ([1,2],[3])]
-- split [1,2,3,4]   -> [([1],[2,3,4]), ([1,2],[3,4]), ([1,2,3],[4])]

-- Split is used by the exprs to generate possible expressions for a list of given numbers

exprs :: [Int] -> [Expr]
exprs []  = []
exprs [n] = [Val n]
exprs ns  = [e | (ls,rs) <- split ns,
                 l       <- exprs ls,
                 r       <- exprs rs,
                 e       <- combine l r]
				 
-- Given a list of integers
-- exprs first splits the list using ns into two halves, ls and rs (returned by split as a tuple)
-- These sides and the repeatedly split until a value is reached 
-- The values are then combined via the combine procedure

-- exprs [1,2]
-- [1+2, 1-2, 1*2, 1/2]


combine :: Expr -> Expr -> [Expr]
combine l r = [App o l r | o <- ops]


-- Combine takes the l/r parts of an expression and returns all possible expressions 
-- for a given set of operations (+,-,*,/)

-- Where an expression can be either a value or another expression 

-- combine (Val 1) (Val 2)
--[1 + 2, 1 - 2, 1 * 2, 1 / 2]

-- combine (App Add (Val 4) (Val 5)) (Val 2)
-- [(4+5)+2,(4+5)-2,(4+5)*2,(4+5)/2]

-- This list of numbers is split to give differing LHS and RHS variants

