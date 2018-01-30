-- Verify that the number of expressions that evaluate successfully
-- increases to 10,839,369 if the numeric domain is generalised 
-- to arbitrary integers


-- Modify the definition of valid 
valid :: Op -> Int -> Int -> Bool
valid Add _ _ = True
valid Sub _ _ = True -- Remove rule of x > y, to allow negative integers
valid Mul _ _ = True
valid Div x y = y /= 0 && x `mod` y == 0 -- Check for 0 division, with no rules on subtraction we can now end up with 0 along with negative numbers (x == y)


-- Same process as question 9.4
all_exps = [e | choice <- choices [1,3,7,10,25,50],
                e <- exprs choice]
				
results = filter (not . null) . map eval all_exps 

length results