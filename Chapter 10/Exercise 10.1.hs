-- Redefine putStr :: String -> IO () using a list comprehension 
-- Also include the library function sequence_ :: [IO a] -> IO () in the procedure

-- putStr :: String -> IO ()
-- putStr [] = return ()
-- putStr (x:xs) = do putChar x
--                   putStr xs

-- Every IO action returns a value, these values are tagged with IO type 
-- This is to help distinguish actions from other values 
-- E.g. getChar :: IO Char 
-- This indicates that getChar when invoked performs some action that returns a character
-- Actions which return no values of interest use the type ()

-- The sequence_ function evaluates every action in a structure and ignores the results


putStr' :: String -> IO ()
putStr' xs = sequence_ [putChar x | x <- xs] -- We'll build a list of putChar actions and then evaluate them all with sequence_		  