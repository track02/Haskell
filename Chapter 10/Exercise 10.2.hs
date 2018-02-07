-- Using recursion define a version of
-- putBoard :: Board -> IO ()
-- that displays nim boards of any size rather than being specific to boards of 5 rows

-- Hint - first define a function that takes the current row number as an additional argument

-- Original definitions
type Board = [Int]

putRow :: Int -> Int -> IO ()
putRow row num = do putStr (show row)
                    putStr ": "
                    putStrLn (concat (replicate num "* "))

putBoard :: Board -> IO ()
putBoard [a,b,c,d,e] = do putRow 1 a
                          putRow 2 b
                          putRow 3 c
                          putRow 4 d
                          putRow 5 e
		  				  

-- Alternatively we can iterate over the board 

-- We'll use a function which takes a board and row number 
showBoard :: Board -> Int -> IO () 
showBoard [x]    n = putRow n x
showBoard (x:xs) n = do putRow n x
                        showBoard xs (n + 1)

-- And a function which sets up the first instance 
putBoard'' :: Board -> IO()
putBoard'' b = showBoard b 1

