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
-- 