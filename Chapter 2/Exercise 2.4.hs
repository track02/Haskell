-- 2.4 write your own implementation of the last function which returns the last element of a list
-- Drop function can be combined with length to find last element
myLast xs = drop ((length xs) - 1) xs