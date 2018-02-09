-- Using getCh define an action readLine :: IO String that behaves in the same way as getLine
-- except that it permits the delete key to be used to remove characters.
-- Hint the delete character is '\DEL' and the control character for moving the cursor back one space is '\b'

import System.IO

-- getCh from hangman example
-- reads a single character from the keyboard without echoing it to the screen

getCh :: IO Char
getCh = do hSetEcho stdin False
           x <- getChar
           hSetEcho stdin True
           return x

readLine :: IO String
readLine = do c <- getCh 
              if c == '\n' then
              	 return []
              else
              	if c == '\DEL' then
              	    do n <- readLine
              	       return ('\b':n)
              	       
              	else
                    do putChar c
                       n <- readLine
                       return (c:n)