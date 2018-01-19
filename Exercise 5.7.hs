gen_double :: [(Int,Int)]
gen_double = concat [[(x,y) | x <- [1,2]] | y <- [3,4]] -- Outer comprehension has access to inner (_,y)




