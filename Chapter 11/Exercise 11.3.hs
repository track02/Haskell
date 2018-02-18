-- Modify the tic-tac-toe program to choose a move
-- that attempts to take the quickest route to win
-- by calculating the depths of resulting game trees
-- and selecting the move that results in a tree with 
-- the smallest depth



-- Update minimax to also label each node
-- with the shortest route of it's best subsequent move
-- (Winning > Drawing > Losing)
minimax :: Tree Grid -> Tree (Grid,Player,Int)                             
minimax (Node g [] _)                                                    
   | wins O g  = Node (g,O,0) [] -- Leaf nodes have child depth of 0, no further nodes to visit                                          
   | wins X g  = Node (g,X,0) []                                          
   | otherwise = Node (g,B,0) []                                          
minimax (Node g ts,)                                                     
   | turn g == O = Node (g, minimum ps, shortest) ts'                             
   | turn g == X = Node (g, maximum ps, shortest) ts'                             
                   where                                                
                      ts' = map minimax ts                              
                      ps  = [p | Node (_,p,_) _ <- ts']
                      shortest = 1 + (minimum [s | Node(_,p,s) _ <- ts', p == turn g] -- TODO Shortest route should be shortest winning route, then drawing route, then losing route
