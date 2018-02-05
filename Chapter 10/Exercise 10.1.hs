Redefine putStr :: String -> IO () using a list comprehension and the library function
sequence_ :: [IO a] -> IO ()

putStr :: String -> IO ()
putStr [] = return ()
putStr (x:xs) = do putChar x
                   putStr xs