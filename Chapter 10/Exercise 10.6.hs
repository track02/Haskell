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

-- Need to handle the following
    -- Building the string
    -- Displaying user input

-- we'll use a helper function that can keep track of the current string
readLine' :: String -> IO String 
readLine' s = do c <- getCh 
                 putChar c -- We'll display the read in character back to the user
                 if c /= '\n' then
                   if c == '\DEL' then -- If delete key is pressed, we need to remove the last element of the string
                     do putChar '\b'  -- We also need to update the display, move cursor back, highlighting last character
                        putChar ' '   -- Replace it with a space
                        putChar '\b'  -- Move back behind the space
						-- abc
						--    ^
						-- abc
						--   ^
						-- ab_
						--    ^
						-- ab_
						--   ^
                        readLine' (init s) -- Init drops the last element
                    else
                      readLine' (s ++ [c]) -- If it's a normal character just add it to the string
                 else
                   return s -- When an endline is reached return the string

-- Wrap the helper function
readLine :: IO String 
readLine = readLine' [] -- Simply calls the helper function with an initially empty string