third_1 :: [a] -> a
third_1 xs = (head (tail (tail xs)))

third_2 :: [a] -> a
third_2 xs = xs !! 2

third_3 :: [a] -> a
third_3 (_:_:z:_) =  z -- (_:_:z:_) is not a tuple, it is parenthesising the cons (:) pattern due to priority of function application
					   -- E.g. third_3 _:_:z:_ would mean (third_3 _) :_:z:_ which is not correct
