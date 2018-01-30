-- Redefine choices using a list comprehension
-- instead of using composition, concat and map

-- choices produces a list of all prossible combinations of a given list of value
-- choices [1,2] -> [[],[2],[1],[1,2],[2,1]]

-- choices is built up using the perms and subs functions 

-- subs takes a list and produces all subsets of a given list 
-- subs [1,2] -> [[],[2],[1],[1,2]]

-- perms takes a list and produces all possible permutations 
-- perms [1,2]   -> [[1,2], [2,1]]
-- perms [1,2,3] -> [[1,2,3],[2,1,3],[2,3,1],[1,3,2],[3,1,2],[3,2,1]]

-- perms itself makes use of the interleaves function which when given a value and a list, produces all 
-- possible interleavings of that value with the given list
-- interleave 3 [1,2] -> [[3,1,2],[1,3,2],[1,2,3]]

-- Combinatorial functions

subs :: [a] -> [[a]]
subs []     = [[]]
subs (x:xs) = yss ++ map (x:) yss
              where yss = subs xs
		  
-- subs [1]    -> subs [] ++ map (1:) subs []
--             -> [[]] ++ map (1:) [[]]
--             -> [[],[1]]

-- subs [1,2]  -> subs [2] ++ map (1:) subs [2]
--             -> ([[]] ++ map (2:) [[]])  ++ map (1:) ([[]] ++ map (2:) [[]])
--             -> ([[],[2]]) ++ [[1],[1,2]]
--             -> [[],[2],[1],[1,2]]                         


interleave :: a -> [a] -> [[a]]
interleave x []     = [[x]]
interleave x (y:ys) = (x:y:ys) : map (y:) (interleave x ys)

-- interleave 3 [1,2]
-- [3,1,2] : map (1:) (interleave 3 [2])
-- [3,1,2] : map (1:) [[3,2],[2,3]]
-- [3,1,2] : [[1,3,2], [1,2,3]]
-- [[3,1,2],[1,3,2],[1,2,3]]

-- interleave 3 [2]
-- [3,2] : map (2:) (interleave 3 [])
-- [3,2] : map (2:) [[3]]
-- [3,2] : [[2,3]]
-- [[3,2],[2,3]

-- interleave 3 []
-- [[3]]

perms :: [a] -> [[a]]
perms []     = [[]]
perms (x:xs) = concat (map (interleave x) (perms xs)) -- concat flattens a list [[a],[b],[c]] -> [a,b,c]

-- perms [1,2,3]
-- concat (map (interleave 1) (perms [2,3])
-- concat (map (interleave 1) [[2,3],[3,2]]
-- [[[1,2,3],[2,1,3],[2,3,1]],[[1,3,2],[3,1,2],[3,2,1]]]
-- [[1,2,3],[2,1,3],[2,3,1],[1,3,2],[3,1,2],[3,2,1]]

-- perms [2,3]
-- concat (map (interleaves 2) (perms [3]))
-- concat (map (interleaves 2) [[3]])
-- [2,3],[3,2]

-- perms [3]
-- concat (map (interleaves 3) (perms []))
-- concat (map (interleaves 3) [[]]
-- [3] (remember concat!)

-- perms []
-- [[]]


-- choices operates by determining all subsets of a list [[subset1], [subset2], [subset3]]
-- and then for each subset calculating a full list of permutations [[[s1p1],[s1p2],[s1p3]], [[s2p1],[s2p2]]]
-- the result is then concatenated [[s1p1],[s1p2],[s1p3],[s2p1],[s2p2]]
choices :: [a] -> [[a]]
choices = concat . map perms . subs

-- list comprehension version draws each subset from the set (s)
-- then draws from the results of determining the permutations of each subset (c) 
choices' :: [a] -> [[a]]
choices' xs = [c | s <- subs xs, 
                   c <- perms s]

