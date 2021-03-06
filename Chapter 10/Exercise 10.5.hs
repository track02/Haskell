-- Redefine adder using the function sequence :: [IO a] -> IO [a] that performs
-- a list of actions and returns a list of the resulting values

import Data.Char


adderSeq :: IO ()
adderSeq = do
           input <- getDigit "How many numbers? " -- Determine no. of values to add 
           total <- adderSeq' input -- Evaluate adder using input and hold result in total 
           putStrLn (show (sum total)) -- Sum the returned IO Int list and display

adderSeq' :: Int -> IO [Int] 
adderSeq' n = sequence [(getDigit "> ") | _ <- [1..n]] -- Create a list of IO actions and evaluate with sequence 



-- We'll use a variant of the getDigit function from the nim example
getDigit :: String -> IO Int 
getDigit prompt = do putStr prompt
                     input <- getLine
                     return (read input :: Int)