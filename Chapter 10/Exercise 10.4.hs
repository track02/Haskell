-- Define an action adder :: IO () that reads a given number 
-- of integers from the keyboard, one per line and displays their sum

-- E.g.
--
-- > adder
-- How many numbers? 5
-- 1
-- 3
-- 5
-- 7
-- 9
-- The total is 25

-- Hint start by defining a function that takes the current total 
-- and how many numbers remain to be read as arguments 

-- You will also need to use the library functions read and show 

import Data.Char


-- We'll use a variant of the getDigit function from the nim example
getDigit :: String -> IO Int 
getDigit prompt = do putStr prompt
                     input <- getLine
                     return (read input :: Int)

adder :: IO ()
adder = do
           input <- getDigit "How many numbers? " -- Determine no. of values to add 
           total <- adder' 0 input -- Evaluate adder using input and hold result in total 
           putStrLn (show total) -- Use show to get string repr and then display 

adder' :: Int -> Int -> IO Int 
adder' t n = do 
                input <- getDigit "> " -- Take input
                if n == 1 then 
			       return (t + input)
			    else
                   adder' (input + t) (n - 1)

              
