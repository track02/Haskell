-- A curried function is a function which takes a single argument
-- and returns either a result or another function which takes another argument
-- All Haskell functions are curried and the -> operator being right-associative demonstrates this
-- a -> b -> c == a -> (b -> c)
-- Function evaluation is left-associative
-- f a b = (f a) b
-- b is applied to the result of (f a)
-- This lines up with the defined type a -> (b -> c)
-- given a, f results in a function that then takes b and returns c
curry' :: ((a,b) -> c) -> (a -> b -> c)
curry' f = \x y -> f (x, y)

uncurry' :: (a -> b -> c) -> ((a,b) -> c)
uncurry' f = \(x,y) -> f x y