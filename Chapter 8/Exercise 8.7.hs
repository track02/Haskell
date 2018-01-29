-- Complete the following

-- Maybe is a type constructor
-- Depending on value type passed to Just
-- The return Maybe type will be Maybe <Type>
-- Just "ABC" :: Maybe [Char]

-- Specify Maybe as an instance of Eq Class
-- Must define a specific implementation of ==
-- for the Maybe type

data Maybe' a = Nothing' | Just' a  

instance Eq a => Eq (Maybe' a) where
    (==) Nothing' Nothing'   = True -- If both elements are nothing, true 
    (==) (Just' a) (Just' b) = a == b -- If both are Just's then compare the values 
    (==) _ _                 = False -- Anything else is False 
		

instance Eq a => Eq [a] where
    (==) [] []         = True 
    (==) (x:xs) (y:ys) = (x == y) && (xs == ys) -- Build up boolean chain comparing element to element 
    (==) _ _           = False