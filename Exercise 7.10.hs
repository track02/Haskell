Using altMap define a function luhn :: [Int] -> Bool
that implements the luhn algorithm from exercise 4.8
for bank card numbers of any length

consider each digit separately

moving left double every other number from the second last

subtract 9 from each number greater than 9

add all numbers together

if total is divisible by 10, vard is value