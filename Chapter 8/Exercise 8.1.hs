-- In a similar manner to add, define a recursive function 
-- Mult for the recursive type of natural numbers
-- Be sure to use add in your definition

-- A natural number can be seen as either
-- Zero or the succession to an existing natural number 
data Nat = Zero | Succ Nat

-- Multiplication can be seen as repeated addition
-- 5 * 3 = 5 + 5 + 5 or 3 + 3 + 3 + 3 + 3
mult :: Nat -> Nat -> Nat 
mult _ Zero = Zero
mult Zero _ = Zero
mult m (Succ n) = add m (mult m n) -- Here step back through the previous value of n and repeat addition

nat2int :: Nat -> Int
nat2int Zero = 0
nat2int (Succ n) = 1 + nat2int n

int2nat :: Int -> Nat
int2nat 0 = Zero
int2nat n = Succ (int2nat (n - 1))

add :: Nat -> Nat -> Nat
add m n = int2nat (nat2int m + nat2int n)