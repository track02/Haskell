-- Using altMap define a function luhn :: [Int] -> Bool
-- that implements the luhn algorithm from exercise 4.8
-- for bank card numbers of any length

-- consider each digit separately

-- moving left double every other number from the second last

-- subtract 9 from each number greater than 9

-- add all numbers together

-- if total is divisible by 10, card is valid

altMap :: (a -> b) -> (a -> b) -> [a] -> [b]
altMap f g [] = []
altMap f g (x:xs) = f x : altMap g f xs

-- Reverse cardnumber and no need to check list length, apply altMap 
-- Then map subtraction function to result 
-- Take sum 
-- Mod result with 10, if 0 then valid 
luhn :: [Int] -> Bool
luhn cardNumber = sum (map subCheck (altMap (+0) (* 2) rev)) `mod` 10 == 0
                  where rev = reverse cardNumber 

subCheck :: Int -> Int 
subCheck x | x > 9 = x - 9
           | otherwise = x 
