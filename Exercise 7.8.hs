-- Test the new transmitter using a faulty channel that drops the first bit
-- this can be modelled using the tail function on a list of bits

*Main> message = "Hello World"

*Main> encoded = encode message
*Main> encoded
[0,0,0,1,0,0,1,0,0,1,0,1,0,0,1,1,0,0,0,0,1,1,0,1,1,0,0,0,0,1,1,0,1,1,0,0,1,1,1,1,0,1,1,0,0,0,0,0,0,0,1,0,0,1,1,1,1,0,1,0
,1,0,1,1,1,1,1,0,1,1,0,0,0,1,0,0,1,1,1,0,0,0,0,1,1,0,1,1,0,0,0,0,1,0,0,1,1,0,1]

*Main> fault_encoded = tail encoded

*Main> decode encoded
"Hello World"

*Main> decode fault_encoded

"*** Exception: Mismatched parity bit!
CallStack (from HasCallStack):
  error, called at Exercise 7.7.hs:68:32 in main:Main
  
*Main>