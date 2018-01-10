Prelude> second xs = head (tail xs)
Prelude> :type second
second :: [a] -> a

Prelude> swap (x, y) = (y, x)
Prelude> :type swap
swap :: (t1, t) -> (t, t1)

Prelude> pair x y = (x, y)
Prelude> :type pair
pair :: t -> t1 -> (t, t1)

Prelude> double x = x * 2
Prelude> :type double
double :: Num a => a -> a

Prelude> palindrome xs = reverse xs == xs
Prelude> :type palindrome
palindrome :: Eq a => [a] -> Bool

Prelude> twice f x = f (f x)
Prelude> :type twice
twice :: (t -> t) -> t -> t

Prelude> :type ['a','b','c']
['a','b','c'] :: [Char]

Prelude> :type ('a','b','c')
('a','b','c') :: (Char, Char, Char)

Prelude> :type [(False, 'O'), (True, '1')]
[(False, 'O'), (True, '1')] :: [(Bool, Char)]

Prelude> :type ([False, True], ['0', '1'])
([False, True], ['0', '1']) :: ([Bool], [Char])

Prelude> :type [[1,2,3],[4,5,6]]
[[1,2,3],[4,5,6]] :: Num t => [[t]]
