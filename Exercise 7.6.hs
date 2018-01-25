A higher-order function unfold encapsulates a simple pattern of recursion
for producing a list can be defined as follows

unfold p h t x | p x = []
               | otherwise = h x : unfold p h t (t x)

That is, unfold p h t produces the empty list if the predicate p
is true of the argument value, and otherwise produces a non-empty list
by applying the function h to this value to give the head
the function t is used to generate another argument that is recursively processed in the same 
way to produce the tail of the list

E.g.

int2bin :: Int -> [Bit]
int2bin 0 = []
int2bin n = n 'mod' 2 : int2bin (n 'div' 2)

int2bin = unfold ( == 0) ('mod' 2) ('div' 2)

Redefine chop8, map f and iterate f using unfold

Where:

import Data.Char

type Bit = Int

chop8 :: [Bit]
chop8 [] = []
chop8 bits = take 8 bits : chop8 (drop 8 bits)

map :: (a -> b) -> [a] -> [b]
map f xs = [f x | x <- xs]

iterate f x = [x, f x, f (f x), f (f (f x)), ...]

