-- Using choices, exprs and eval
-- Verify that there are 33,665,406 possible expressions
-- over the numbers 1,3,7,10,25,50 and that only
-- 4672540 evaluate successfully

-- Get all choices 
-- For each choice calculate the expressions
-- Store each expression 

all_exps = [e | choice <- choices [1,3,7,10,25,50],
                e <- exprs choice]
				
length all_exps
-- 33,665,406 

-- Now need to evaluate these expressions 
-- Map each expression to the result of evaluation
-- Filter out any invalid expressions which return [], (can check using null for an empty list )

results = filter (not . null) . map eval all_exps 

length results
--4672540