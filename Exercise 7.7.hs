Modify the binary transmitter example to detect simple transmission errors using parity bits

Each eight-bit binary number produced is extended with a parity bit, set to one if the 
number contains an odd number of ones and to zero otherwise

Each of these nin-bit binary numbers consumed during decoding is checked to ensure
the parity bit is correct, with the p.bit being discarded if correct and an error reported otherwise

Hint - library function error :: String -> a displays the given string as a message 
The polymorphic result type ensures error can be used in any context

