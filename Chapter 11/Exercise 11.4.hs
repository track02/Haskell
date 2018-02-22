-- Modify the tic-tac-toe program to:

-- let the user decide if they want to play first or second 
-- allow the length of a winning line to be modified
-- generate the game tree once, rather than for each move
-- reduce the size of the game tree using alpha-beta pruning

import Data.Char
import Data.List
import System.IO

data Player = O | B | X
              deriving (Eq, Ord, Show)

type Grid = [[Player]]

size = 3

-- 1) Let user decide to go first or second 
-- To achieve this we'll create a function which
-- prompts the user for their desired turn and
-- return an X or O appropriately 
firstPlayer :: IO Player
firstPlayer = do turn <- getNat "Go first or second? (1 / 2)\n> "
                 if turn == 1 then return O else return X -- Consider adding validation check if turn > 2 || < 1

getNat :: String -> IO Int
getNat prompt = do putStr prompt
                   xs <- getLine
                   if xs /= [] && all isDigit xs then
                      return (read xs)
                   else
                      do putStrLn "ERROR: Invalid number"
                         getNat prompt

-- 2) Allow the length of a winning line to be modified 
-- To do this we need another value to represent
-- the length of a winning line 

winLength = 2

-- We then need to chop rows / cols / diags into sub-lists 
-- of this length 
-- E.g. [[O,X,X],[X,O,O],[B,B,B]
-- Would become [[O,X],[X,X], [X,O],[O,O], [B,B],[B,B]]

-- We'll make a new procedure to split up a line list 
chopLines :: [[Player]] -> [[Player]]
chopLines [] = []
chopLines (x:xs) = (chop winLength x) ++ chopLines xs 

-- We'll modify chop slightly to take a length then drop only one element 
chop :: Int -> [a] -> [[a]]
chop n [] = []
chop n xs |length xs < n = []  -- Stop when there are no more elements to take 
          |otherwise = take n xs : chop n (drop (n - 1) xs)

-- Testing 		  
-- chopLines [[X,O,X],[O,O,X],[B,B,B]]
-- [[X,O],[O,X],
--  [O,O],[O,X],
--  [B,B],[B,B]]

-- And update the wins procedure to split each returned [[Player]] list into futher sublists of length winLength 
wins :: Player -> Grid -> Bool
wins p g = any line (rows ++ cols ++ dias)
           where
              line = all (== p)
              rows = chopLines g
              cols = chopLines $ transpose g
              dias = chopLines [diag g, diag (map reverse g)]

-- wins O [[X,O,X],[O,O,X],[B,B,B]]
-- True

diag :: Grid -> [Player]
diag g = [g !! n !! n | n <- [0..size-1]]