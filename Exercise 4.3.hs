safetail_cond :: [a] -> [a]
safetail_cond xs = if null xs then [] else tail xs

safetail_guard :: [a] -> [a]
safetail_guard xs | null xs = [] 
                  | otherwise = tail xs

safetail_pattern :: [a] -> [a]
safetail_pattern [] = []
safetail_pattern (_:xs) = xs -- Remember a list can be broken down using cons op
							 -- [1,2,3] = 1 : (2 : (3 : []))
							 -- This pattern match says to return everything but the first element (2 : (3 : [])) {[2,3]}