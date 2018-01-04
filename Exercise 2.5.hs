-- 2.4 the init function removes the last element from a list
-- write two implementations for init using the defined library functions

firstInit xs = take ((length xs) - 1) xs

secondInit xs = reverse (drop 1 (reverse xs))