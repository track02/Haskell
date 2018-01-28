dec2int :: [Int] -> Int
dec2int = foldl (\x y -> (10 * x)+ y)) 0
-- Here x is the accumulated value so far, starting from initial (0)
-- Unlike foldr where this value is added at the end
-- Multiply it by 10 for each element starting at zero
-- Then add the element
-- [1,2,3]
-- (0 * 10) + 1) * 10) + 2) * 10) + 3)
-- (1) * 10) + 2) * 10) + 3)
-- (12) * 10) + 3)
-- 123


