-- In a similar manner to exercise 1, redefine the generalised version of putBoard
-- using a list comprehension and sequence_

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

-- We can use a list comprehension to generate a list of putRow actions
-- Zipping the board against an infinite list will produce a list of tuples containing (row / num)
-- We can then draw from this list to create putRow actions 
-- The sequence_ method can then be used to iterate over this action list and evaluate each putRow action 
-- This results in the board being displayed to output
						  
putBoard' :: Board -> IO ()
putBoard' xs = sequence_ [putRow r n | (r,n) <- zip [1..] xs] 