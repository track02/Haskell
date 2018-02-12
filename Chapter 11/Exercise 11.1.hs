-- Using the function gametree verify that there are
-- 549,946 nodes in the complete game tree for 
-- a 3x3 tic-tac-toe game, starting from the empty grid
-- and that the maximum depth of this tree is 9

-- Pull over elements from example
import Data.Char
import Data.List
import System.IO


type Grid = [[Player]] -- Game grid, a list of lists of Player (O,B,X)

size :: Int
size = 3


data Player = O | B | X  -- Entries into each cell 
              deriving (Eq, Ord, Show)
			  
data Tree a = Node a [Tree a]
              deriving Show

empty :: Grid  -- Creates an empty 3x3 grid (all blanks)
empty = replicate size (replicate size B)
			  
next :: Player -> Player -- Given a player determine who goes next 
next O = X
next B = B
next X = O			  

full :: Grid -> Bool -- Concat (flattens) grid and check no blanks are present 
full = all (/= B) . concat

won :: Grid -> Bool -- Given a grid check if theres a winner 
won g = wins O g || wins X g 

wins :: Player -> Grid -> Bool 
wins p g = any line (rows ++ cols ++ dias) -- Create a list of all rows, cols and diags then apply lines to check if any are winners for the player p
           where
              line = all (== p) -- check if a all elements of a line == given player (p)
              rows = g -- each row (grid itself)
              cols = transpose g -- transposes grid (E.g. [[1,2,3],[4,5,6]] == [[1,4],[2,5],[3,6]])
              dias = [diag g, diag (map reverse g)] -- Gets both diagonals (0,0 - NN) (0,N - N,0)
			  
diag :: Grid -> [Player] -- Given a grid return the diagonal cells as a list 
diag g = [g !! n !! n | n <- [0..size-1]] -- Creates a list by indexing at 0,0 - 1,1 - 2,2 - 3,3 - N,N 

moves :: Grid -> Player -> [Grid] -- Determines all possible moves for a given grid and player 
moves g p | won g     = [] -- No more moves if won 
          | full g    = [] -- No more moves if grid is full 
          | otherwise = concat [move g i p | i <- [0..((size^2)-1)]] -- Iterates over grid and checks a move in each cell, invalid moves return an empty list 

move:: Grid -> Int -> Player -> [Grid] -- Given a grid, cell no. and player determine if move is valid 
move g i p =
   if valid g i then [chop size (xs ++ [p] ++ ys)] else [] -- Where a move is valid, recreate the grid with this move and return otherwise return false 
   where (xs,B:ys) = splitAt i (concat g) -- flatten grid and split it at i where the move occurs 
   
valid :: Grid -> Int -> Bool -- Checks if a given move is valid 
valid g i = 0 <= i && i < size^2 && concat g !! i == B -- move falls on grid (between 0 - size) and the cell is blank
   
chop :: Int -> [a] -> [[a]] -- Takes a list and chops it into sublists
chop n [] = []
chop n xs = take n xs : chop n (drop n xs)
			  
gametree :: Grid -> Player -> Tree Grid  -- Given a grid and player, return a tree of grids representing all possible moves 
gametree g p = Node g [gametree g' (next p) | g' <- moves g p] -- generate moves for player, call gametree for each of these moves and the next player 
                                                               -- this repeats generating grids for every possible combination of moves 

-- We need a function that can count the no. Nodes in a tree 
-- Where a Tree consists of a Node holding a values (a) and a list of following trees 
-- So for a given tree we need to count it's node and then count all the nodes of the following trees 

-- Count up nodes 
treeCount :: Tree Grid -> Int 
treeCount (Node _ ns) = 1 + sum [treeCount n | n <- ns]
					
-- Count depth 					
depthCount :: Tree  Grid -> Int 
depthCount (Node _ ns) = maximum [depthTrack 1 n | n <- ns]

depthTrack :: Int -> Tree Grid -> Int 
depthTrack i (Node _ []) = i
depthTrack i (Node _ ns) = maximum [depthTrack (i + 1) n | n <- ns]

-- Alternatively
depthCount' :: Tree Grid -> Int 
depthCount' (Node _ []) = 0 
depthCount' (Node _ ns) = 1 + maximum [depthCount' n | n <- ns]


-- tree = gametree empty 0
-- treeCount tree
-- 549946
-- depthCount tree 
-- 9
