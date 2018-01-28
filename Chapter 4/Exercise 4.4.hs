_ || True = True

True || _ = True

False || False = False

b || c | b == c = b
       | otherwise = True