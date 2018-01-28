-- Define a function

altMap :: (a -> b) -> (a -> b) -> [a] -> [b]
--           ^ f1        ^ f2      ^ list ^ result

altMap f g [] = []
altMap f g (x:xs) = f x : altMap g f xs -- Just swap functions around with each call to alternate 


-- That alternately applies its two argument functions to successive elements in a list
-- altmap (+10) (+100) [0,1,2,3,4]

-- [10,101,12,103,14]
--  f   g   f  g   f
