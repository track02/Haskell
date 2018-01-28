import Data.Char

type Bit = Int

-- A higher-order function unfold encapsulates a simple pattern of recursion
--for producing a list can be defined as follows

unfold p h t x | p x = []
               | otherwise = h x : unfold p h t (t x)

-- That is, unfold p h t produces the empty list if the predicate p
-- is true of the argument value, and otherwise produces a non-empty list
-- by applying the function h to this value to give the head
-- the function t is used to generate another argument that is recursively processed in the same 
-- way to produce the tail of the list

-- p predicate used to terminate list creation when argument is true 
-- h function used to generate the head of the list
-- t used to generate another argument that is recursively processed to produce the tail of the list 

--E.g.

-- int2bin :: Int -> [Bit]
-- int2bin 0 = []
-- int2bin n = n 'mod' 2 : int2bin (n 'div' 2)



int2bin :: Int -> [Bit]
int2bin = unfold ( == 0) (`mod` 2) (`div` 2)

-- Redefine chop8, map f and iterate f using unfold
-- Where:

chop8 :: [Bit] -> [[Bit]]
chop8 [] = []
chop8 bits = take 8 bits : chop8 (drop 8 bits)

chop8' :: [Bit] -> [[Bit]]
-- p, null, checks if a list is empty
-- h, take 8, remove 8 bits to generate current head item
-- t, drop 8, generates next item to be processed
chop8' = unfold null (take 8) (drop 8)


map' :: (a -> b) -> [a] -> [b]
map' f xs = [f x | x <- xs]

unfoldmap :: (a -> b) -> [a] -> [b]
-- p, null, checks if a list is empty 
-- h, f, applied given function (a -> b) to head 
-- t, drop 1, move to next element in list 
unfoldmap f = unfold null (f . head) (drop 1)

-- iterate f x = [x, f x, f (f x), f (f (f x)), ...]
-- generates an infinite list of f repeatedly applied to x 
unfolditerate :: (a -> a) -> a -> [a]
--                  ^ f      ^ x   ^ infinite list 

-- p, false, never want list to stop being generated 
-- h, no change, use identity function to not change x 
-- t, apply f to x, -> [1, f 1, f (f 1), f (f (f 1 )), ...] f x is re-used on next evaluation 
-- h x : unfold p h t (t x)
unfolditerate f = unfold (const False) id f

