-- Tic-tac-toe example from chapter 11 of Programming in Haskell,
-- Graham Hutton, Cambridge University Press, 2016.

-- Basic declarations

-- Annotated Version


import Data.Char
import Data.List
import System.IO

size :: Int -- Size of our board, 3 x 3 Grid
size = 3

type Grid = [[Player]] -- Grid itself is a list of player lists
                       --         X | B | O
                       --         B | X | O
                       --         X | O | B
                       -- Where each row of the grid is a list

data Player = O | B | X -- Player is either O, B or X, used to represent a mark on the grid
              deriving (Eq, Ord, Show)

next :: Player -> Player -- next is used to determine which player goes next, basically swaps between O <-> X
next O = X
next B = B
next X = O

-- Grid utilities

empty :: Grid 
empty = replicate size (replicate size B) -- Initialises an empty grid of blank players
                                          -- Replicate returns a list of the value repeated n times
                                          -- So here make a list of 3 Blanks and then a list of three blank lists

full :: Grid -> Bool                       
full = all (/= B) . concat                -- full checks if a grid is full
                                          -- concat flattens the grid list, [[O,X,O],[X,O,X],[O,X,X]]
                                          -- would become [O,X,O,X,O,X,O,X,X]
                                          -- all is used to check if all marks satisfy the condition
                                          -- namely they are not blank

turn :: Grid -> Player                        -- turn determines which player goes next on a given grid 
turn g = if os <= xs then O else X            -- os is the total number of O's on the grid
         where                                -- xs is the total number of X's on the grid
            os = length (filter (== O) ps)    -- if less O's than X's then it's O's turn
            xs = length (filter (== X) ps)    -- totals are determined by flattening grid
            ps = concat g                     -- and taking length of the listed filtered by each mark


wins :: Player -> Grid -> Bool                        -- wins determines if a given player
wins p g = any line (rows ++ cols ++ dias)            -- has won on a given grid
           where                                      -- It creates three lists
              line = all (== p)                       -- rows which are the three horizontal rows of the grid
              rows = g                                -- cols which are the three vertical columns of the grid
              cols = transpose g                      -- dias which are the two diagonals (0,0 - 3,3) and (3,0 - 0,3) 
              dias = [diag g, diag (map reverse g)]   -- for each of these lists all is applied to check if every mark
                                                      -- in the last matches the given player (p) 
                                                      -- if any of these lists return true for this check then the player
                                                      -- (p) has won the game, returning True otherwise False

                                                      -- Notes
                                                      -- transpose interchanges rows and colums
                                                      -- transpose [[1,2,3],[4,5,6]] == [[1,4],[2,5],[3,6]]
                                                      -- take diag of g and then reverse it to get the opposite / -> \



diag :: Grid -> [Player]                              -- diag is used by wins to create a list of diagonal marks
diag g = [g !! n !! n | n <- [0..size-1]]             -- diag simply counts up and indexes the grid at (0,0), (1,1)
                                                      -- (2,2) and (3,3) and returns a list of these players



won :: Grid -> Bool                                   -- won simply determines if a game has been won
won g = wins O g || wins X g                          -- by either O or X

-- Displaying a grid

putGrid :: Grid -> IO ()                                            -- putGrid is used to display the grid on screen
putGrid =                                                           -- It converts all rows to a string representation for    
   putStrLn . unlines . concat . interleave bar . map showRow       -- display by mapping showRow
   where bar = [replicate ((size*4)-1) '-']                         -- It then interleaves each row with a bar "----------"
                                                                    -- This list is then flattened via concat
                                                                    -- ["<row>","<bar>", "<row>", ...]
                                                                    -- unlines is then applied which joins all the
                                                                    -- row and bar strings together placing a newline between each
                                                                    -- "<row>\n<bar>\n<row>..."
                                                                    -- This final string is then displayed using putStrLnS

showRow :: [Player] -> [String]                                     -- For a list of players (a row) [O,X,O]
showRow = beside . interleave bar . map showPlayer                  -- map each player to a string list (showPlayer)
          where                                                     -- [[" ", "O", " "], [" ", "X", " "], [" ", "O", " "]]
             beside = foldr1 (zipWith (++))                         -- Interleave each player string with a bar string ["|","|","|"]
             bar    = replicate 3 "|"                               -- [[" ", "O", " "], ["|","|","|"], ...]
                                                                    -- Then folds from right applying zipWith (++)
                                                                    -- zipWith pairs up elements and then applies the function (++) to them
                                                                    -- foldr1 does not operate on an empty list
                                                                    -- So we end up with the following after applying foldr1 (zipWith (++)) to the list
                                                                    -- [" ","O"," "] zipWith ["|","|","|"] zipWith [" ","X"," "] zipWith ["|","|","|""] zipWith [" ","O"," "]
                                                                    -- Which evaluates to
                                                                    -- [" | | ",
                                                                    --  "O|X|O",
                                                                    --  " | | "]
                                                                    -- Giving us a list string representing the row
                                                                    -- [" | | ", "O|X|O", " | | "]

                                                                    -- Note: Strings are of type [char] so (++) operator can
                                                                    -- be applied to them using zipWith
                                                                    -- "abc" ++ "def" ++ [] ++ [] -> "abcdef"


showPlayer :: Player -> [String]                                    -- Convert a player (mark)
showPlayer O = ["   ", " O ", "   "]                                -- Into a list of strings
showPlayer B = ["   ", "   ", "   "]                                -- containing a space, the mark and a space
showPlayer X = ["   ", " X ", "   "]

interleave :: a -> [a] -> [a]                                       -- interleave interleaves a given value
interleave x []     = []                                            -- inbetween elements of a given list
interleave x [y]    = [y]                                           -- interleave "_" ["O", "O", "O"]
interleave x (y:ys) = y : x : interleave x ys                       -- ["O","_","O","_","O"]
                      

-- Making a move

valid :: Grid -> Int -> Bool                                        -- valid tests if a move is valid
valid g i = 0 <= i && i < size^2 && concat g !! i == B              -- we can number each cell of the grid
                                                                    -- and access it by indexing the concat grid
                                                                    -- valid checks if the chosen grid cell i
                                                                    -- is within the range 0 - 9 (size ^2)
                                                                    -- and is currently blank

move:: Grid -> Int -> Player -> [Grid]                              -- Given a grid, cell number and player (mark)
move g i p =                                                        -- If the move is valid
   if valid g i then [chop size (xs ++ [p] ++ ys)] else []          -- Replace the selected blank cell with the player
   where (xs,B:ys) = splitAt i (concat g)                           -- and return the updated grid wrapped in a list
                                                                    -- Flatten grid into single list
                                                                    -- [O,X,B,X,O,O,X,X,O]
                                                                    -- split it at chosen point giving a tuple (xs,B:ys)
                                                                    -- splitAt 2 [O,X,B,X,O,O,X,X,O]
                                                                    -- ([O,X],[B,X,O,O,X,X,O])
                                                                    -- replace B with chosen mark and append lists, p = X
                                                                    -- [O,X,X,X,O,O,X,X,O]
                                                                    -- Now chop this list into 3's (size) so it
                                                                    -- represents a grid again [[player]]
                                                                    -- [[O,X,X],[X,O,O],[X,X,O]]
                                                                    -- Result is then wrapped in a list
                                                                    -- [[[O,X,X],[X,O,O],[X,X,O]]]
                                                                    -- is a singleton list is returned, move successfully applied
                                                                    -- if an empty list is return, move unsucessful


chop :: Int -> [a] -> [[a]]                                         -- Given a number n and a list xs
chop n [] = []                                                      -- split up the list into sublists
chop n xs = take n xs : chop n (drop n xs)                          -- of size n, by taking n and 
                                                                    -- applying chop to xs with n elements dropped
                                                                    -- chop 3 [1,2,3,4,5,6]
                                                                    -- [[1,2,3],[4,5,6]]

-- Reading a natural number

getNat :: String -> IO Int                                          -- Requests a number from the user
getNat prompt = do putStr prompt                                    -- used for selecting a grid cell
                   xs <- getLine                                    -- first displays a prompt using putStr
                   if xs /= [] && all isDigit xs then               -- Then reads in input from getLine
                      return (read xs)                              -- if input is not empty and is a digit
                   else                                             -- evaluate it to an Int and return
                      do putStrLn "ERROR: Invalid number"           -- Otherwise display error message
                         getNat prompt                              -- and try again

-- Human vs human

tictactoe :: IO ()                                                  -- begins a game of tictactoe
tictactoe = run empty O                                             -- with an empty board and player O 
                                                                    -- going first

run :: Grid -> Player -> IO ()                                      -- displays a given game grid                                  
run g p = do cls                                                    -- and calls game logic function run'
             goto (1,1)                                               
             putGrid g
             run' g p

run' :: Grid -> Player -> IO ()                                     -- game logic function takes a game grid and current player
run' g p | wins O g  = putStrLn "Player O wins!\n"                  -- firstly checks if there's a winner or a draw
         | wins X g  = putStrLn "Player X wins!\n"                  -- If so, display appropraite message and end game
         | full g    = putStrLn "It's a draw!\n"                    --
         | otherwise =                                              -- Otherwise, asks current player where they'd
              do i <- getNat (prompt p)                             -- like to make a move
                 case move g i p of                                 --
                    []   -> do putStrLn "ERROR: Invalid move"       -- if move is invalid (empty list returned) then try again
                               run' g p                             --
                    [g'] -> run g' (next p)                         -- Otherwise call run with the new grid to redisplay
                                                                    -- the board and swap to next player

prompt :: Player -> String                                          -- Given a player O,X
prompt p = "Player " ++ show p ++ ", enter your move: "             -- create a prompt asking for their move

cls :: IO ()                                                            -- cls clears the screen
cls = putStr "\ESC[2J"                                                  -- using the \ESC[2J control character

goto :: (Int,Int) -> IO ()                                              -- the position of each character on screen
goto (x,y) = putStr ("\ESC[" ++ show y ++ ";" ++ show x ++ "H")         -- is given by a pair of integers, with (1,1) being
                                                                        -- the top-left corner. we use goto to move
                                                                        -- the cursor to a specified position

-- Game trees

data Tree a = Node a [Tree a]                                           -- A recursive tree structure
              deriving Show                                             -- Where each node consists of a value
                                                                        -- and a list of trees of the same type
                                                                        --
                                                                        -- a leaf node is represented by a node
                                                                        -- with an empty tree list
                                                                        --
                                                                        --
                                                                        --                   10
                                                                        --                  /  \ 
                                                                        --                 4    12
                                                                        --                /       \
                                                                        --               2         15
                                                                        --
                                                                        --
                                                                        -- Node 10 [Node 4 [Node 2 []], Node 12 [Node [15]]]
                                                                        --


gametree :: Grid -> Player -> Tree Grid                                 -- Given a grid and a player
gametree g p = Node g [gametree g' (next p) | g' <- moves g p]          -- forms a gametree (Tree Grid)
                                                                        -- which shows all possible moves 
                                                                        -- starting from the initial grid
                                                                        -- Root node is initial grid
                                                                        -- With children consisting of all the possible
                                                                        -- generated moves for the initial grid and initial player
                                                                        -- their children are the moves of the next player on each 
                                                                        -- of these new grids
                                                                        --
                                                                        --                  Starting_Grid        (Initial call with Grid and Player O produces root)
                                                                        --                 /     |       \
                                                                        --             Move_1   Move_2   Move_3  (Adding Player O Move's to Grid)
                                                                        --            /   \     /   \      |
                                                                        --          Mv_1 Mv_2  Mv_1 Mv_2  Mv_1   (Adding Player X Move's to each of O's moves) 
                                                                        --           |             /  \      
                                                                        --         Mv_1           Mv_1 Mv_2      (Adding Player O Move's to each of X's moves)
                                                                        --                   ......


moves :: Grid -> Player -> [Grid]                                        -- Given a grid and player determine all possible moves
moves g p | won g     = []                                               -- If grid is won or full, empty list no more moves
          | full g    = []                                               -- Otherwise iterate over all grid cells (0 .. 9)
          | otherwise = concat [move g i p | i <- [0..((size^2)-1)]]     -- and try making moves using move which returns [Grid]
                                                                         -- store returned moves in a list [[Grid]]
                                                                         -- and concat [Grid]
                                                                         -- These moves are used for form the tree lists for a node

prune :: Int -> Tree a -> Tree a                                         -- prune is used to reduce a tree to a specified depth
prune 0 (Node x _)  = Node x []                                          -- a tree of depth 0 has no children
prune n (Node x ts) = Node x [prune (n-1) t | t <- ts]                   -- otherwise for a given root node and depth
                                                                         -- traverse its children and prune them
                                                                         -- decreasing counter each time
                                                                         -- at a depth of zero the children of a node are replaced with the empty list
                                                                         -- which ends the tree at that level

depth :: Int                                                             -- depth of pruning to perform when game is running
depth = 9                                                                -- 9 is max possible tree depths as there 
                                                                         -- are only 9 cells to fill on a grid

-- Minimax

minimax :: Tree Grid -> Tree (Grid,Player)                              -- Converts a tree into a labelled minimax tree
minimax (Node g [])                                                     -- Where a node now holds a pair (Grid, Player)
   | wins O g  = Node (g,O) []                                          -- and a list of (Grid, Player) Nodes
   | wins X g  = Node (g,X) []                                          -- If a node has no children, it's an end state
   | otherwise = Node (g,B) []                                          -- so annotate it with the winner O or X
minimax (Node g ts)                                                     -- or B if it's a draw
   | turn g == O = Node (g, minimum ps) ts'                             -- Where a node does have children
   | turn g == X = Node (g, maximum ps) ts'                             -- recursively apply minimax until a leaf is reached
                   where                                                -- the label applied to the node (O,X) then depends on
                      ts' = map minimax ts                              -- the minimum or maximum of the labels of it's children
                      ps  = [p | Node (_,p) _ <- ts']                   -- Remember O < B < X as defined in the Player type
                                                                        -- if it's O's turn minimum is taken
                                                                        -- if it's X's turn maximum is taken
                                                                        --
                                                                        --
                                                                        --                 (?)
                                                                        --                / | \
                                                                        --               O  B  B
                                                                        --
                                                                        -- If it was O's turn then the minimum of the children
                                                                        -- [O, B, B] is O, a win is possible
                                                                        --
                                                                        --                  O
                                                                        --                / | \
                                                                        --               O  B  B         
                                                                        --
                                                                        -- But if it's X's turn then the maximum is B, only a draw is possible
                                                                        --
                                                                        --                  B
                                                                        --                / | \
                                                                        --               O  B  B   
                                                                        --
                                                                        -- 
                                                                        --
                                                                        -- So the preference is Win, Draw, Loss
                                                                        -- this is controlled for each player by using either minimum or maximum
                                                                        -- depending on the player value
                                                                        --
                                                                        -- This labelling process allows us to determine 
                                                                        -- the result of taking a certain move as we can see it's best possible outcome
                                                                        -- in advance                                                                      
                                                                         
bestmove :: Grid -> Player -> Grid                                      -- Given a grid and current player the bestmove function
bestmove g p = head [g' | Node (g',p') _ <- ts, p' == best]             -- picks the best possible move the current player can make
               where                                                    -- by first generating a minimax tree
                  tree = prune depth (gametree g p)                     -- and from the root, examine it's children (possible moves)
                  Node (_,best) ts = minimax tree                       -- and select the first which has a label matching the current player
                                                                        -- as this move leads to grid on the path to win
-- Human vs computer

main :: IO ()                                                           -- Disables output buffering
main = do hSetBuffering stdout NoBuffering                              -- and begins a game on an empty grid
          play empty O                                                  -- with player O going first 

play :: Grid -> Player -> IO ()                                         -- Same as run function above, displays the grid 
play g p = do cls                                                       -- and calls play' which handles logic
              goto (1,1)
              putGrid g
              play' g p

play' :: Grid -> Player -> IO ()                                       -- Similar to run' above
play' g p                                                              -- checks if there's a winner or draw
   | wins O g = putStrLn "Player O wins!\n"                            -- if not takes next move
   | wins X g = putStrLn "Player X wins!\n"                            --
   | full g   = putStrLn "It's a draw!\n"                              --
   | p == O   = do i <- getNat (prompt p)                              -- prompt player for input and apply move if valid
                   case move g i p of                                  -- otherwise retry taking input
                      []   -> do putStrLn "ERROR: Invalid move"        -- once applied pass the updated grid back to play to be displayed
                                 play' g p                             -- and swap to next player X
                      [g'] -> play g' (next p)                         --
   | p == X   = do putStr "Player X is thinking... "                   -- Computer is controlling player X
                   (play $! (bestmove g p)) (next p)                   -- so use bestmove to determine computers move 
                                                                       -- and use resulting grid to call play to display the grid 
                                                                       -- and swap to the next player O  