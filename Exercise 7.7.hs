-- Modify the binary transmitter example to detect simple transmission errors using parity bits

-- Each eight-bit binary number produced is extended with a parity bit, set to one if the 
-- number contains an odd number of ones and to zero otherwise

-- Each of these nine-bit binary numbers consumed during decoding is checked to ensure
-- the parity bit is correct, with the p.bit being discarded if correct and an error reported otherwise

-- Hint - library function error :: String -> a displays the given string as a message 
-- The polymorphic result type ensures error can be used in any context

import Data.Char

-- Base conversion

type Bit = Int

bin2int :: [Bit] -> Int
bin2int = foldr (\x y -> x + 2*y) 0

int2bin :: Int -> [Bit]
int2bin 0 = []
int2bin n = n `mod` 2 : int2bin (n `div` 2)

-- make8 pads a set of bits with 0's
make8 :: [Bit] -> [Bit]
make8 bits = take 8 (bits ++ repeat 0)

-- Transmission
-- Encodes each character in a string ([Char])
encode :: String -> [Bit]
-- Update encode to add a parity bit to each 8-bit message 
-- Convert char to int, int to binary, pad to 8-bits, add parity bit 
encode = concat . map (addparitybit . make8 . int2bin . ord)


-- chop8 updated to chop9 instead to account for parity bit when decoding
chop9 :: [Bit] -> [[Bit]]
chop9 []   = []
chop9 bits = take 9 bits : chop9 (drop 9 bits)

decode :: [Bit] -> String
-- Decode should take the parity bit and compare it to with the received 8-bit message 
-- If match is okay then convert message to character
-- Otherwise throw an error (error "Mismatched parity bit!")
-- First check parity bit - throw error if its incorrect
-- Otherwise message is returned and conversion procedure begins
decode = map (chr . bin2int. checkparity) . chop9

transmit :: String -> String
transmit = decode . channel . encode

channel :: [Bit] -> [Bit]
channel = id

-- Parity Bit 
-- Given a list of bits add a parity bit 
-- If sum of 1's is odd set to 1 else set to 0

addparitybit :: [Bit] -> [Bit]
addparitybit bits = bits ++ [findparitybit bits]

findparitybit :: [Bit] -> Bit
findparitybit bits = if odd (sum bits) then 1 else 0

checkparity :: [Bit] -> [Bit]
checkparity bits | findparitybit message == parity = message
                 | otherwise = error "Mismatched parity bit!"
                 where
                 message = take 8 bits
                 parity = last bits 