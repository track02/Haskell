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

data Tree a = Node a [Tree a]
              deriving Show

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

-- 3) Generate the game tree once, rather than for each move
-- If a single game tree were to be used for the entire game
-- A complete tree is generated at the start of the game, after player order is decided
-- Each turn would now need to accept a tree representing the current state of
-- the grid and the current players possible moves
-- When a move is made the tree is 'updated' (root becomes selected move node)
-- At the end of each turn this updated tree is returned to be used by the following turn

-- 1) Generate a full tree at start of game 
-- 2) This tree become a parameter of play and play' 
-- 3) When it's the players turn, match their move to the children of the root 
--    a) This child then becomes the root of the tree
-- 4) When it's the ai's turn it uses the tree to select its next move 
--    a) This child then becomes the root of the tree 
--
-- The tree is reduced with each turn depending on the move taken 
-- Rather than being regenerated with each AI turn 

main :: IO ()
main = do hSetBuffering stdout NoBuffering
          play empty O (minimax  (gametree empty O)) -- Now generate a complete game tree


play :: Grid -> Player ->  Tree (Grid,Player) -> IO () -- Play now takes in a gametree 
play g p t = do cls
              goto (1,1)
              putGrid g
              play' g p t 

play' :: Grid -> Player -> Tree (Grid, Player) -> IO () -- Play' also takes in a tree
play' g p t
   | wins O g = putStrLn "Player O wins!\n"
   | wins X g = putStrLn "Player X wins!\n"
   | full g   = putStrLn "It's a draw!\n"
   | p == O   = do i <- getNat (prompt p)
                   case move g i p of
                      []   -> do putStrLn "ERROR: Invalid move"
                                 play' g p
                      [g'] -> play g' (next p) (reduceTree t g')
   | p == X   = do putStr "Player X is thinking... "
                   play g' (next p) (reduceTree t g') -- The tree is reduced depending on the move selected 
				   where g' = (bestmove g p t) -- bestmove now uses the tree to pick a move 

-- Best move would also be updated to take in the current tree
bestmove :: Player -> Tree (Grid, Player) -> Grid
bestmove p (Node (_,best) ts) = head [g' | Node (g',p') _ <- ts, p' == best]
				   
-- Given a tree and a grid find the child with that grid and return 
reduceTree :: Tree (Grid, Player) -> Grid -> Tree (Grid, Player) 
reduceTree (Node x ts) g = head [x | x <- ts, g == (treeGrid x)]

-- Given a node retrieve it's grid 
treeGrid :: Tree (Grid, Player) -> Grid
treeGrid (Node (g,_) _) = g 

-- Testing tree reduction 
-- *Main> t = gametree [[O,O,B],[X,X,B],[X,X,B]] O
-- *Main> mt = minimax t
-- *Main> reduceTree mt [[O,O,O],[X,X,B],[X,X,B]]
-- Node ([[O,O,O],[X,X,B],[X,X,B]],O) []

-- Testing new best move 
-- *Main> t = gametree [[O,O,B],[X,X,B],[X,X,B]] O
-- *Main> mt = minimax t
-- *Main> bestmove' O mt
-- [[O,O,O],[X,X,B],[X,X,B]]


-- 4) Alpha-beta pruning
-- Alpha-beta pruning seeks to decrease the number of nodes evaluated by the 
-- minimax algorithm in its search tree.
-- 
--The algorithm maintains two values, alpha and beta,
--which represent the minimum score that the maximizing player is assured of 
--and the maximum score that the minimizing player is assured of respectively.
--
--Initially alpha is negative infinity and beta is positive infinity,
-- i.e. both players start with their worst possible score.
--
--Whenever the maximum score that the minimizing player is assured 
--of becomes less than the minimum score that 
--the maximizing player is assured of (i.e. beta <= alpha), 
--the maximising player need not consider the descendants 
--of this node as they will never be reached in actual play.

-- function alphabeta(node, depth, α, β, maximizingPlayer)
--      if depth = 0 or node is a terminal node
--          return the heuristic value of node
--      if maximizingPlayer
--          v := -∞
--          for each child of node
--              v := max(v, alphabeta(child, depth – 1, α, β, FALSE))
--              α := max(α, v)
--              if β ≤ α
--                  break (* β cut-off *)
--          return v
--     else
--          v := +∞
--          for each child of node
--              v := min(v, alphabeta(child, depth – 1, α, β, TRUE))
--              β := min(β, v)
--              if β ≤ α
--                  break (* α cut-off *)
--          return v