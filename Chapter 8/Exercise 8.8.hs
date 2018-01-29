-- Extend the tautology checker to support the use of logical disjunction (V) and equivalence (<=>) in propositions
-- V is the equivalent of OR, i
-- <=>, A if and only if B, logical value of A must match logical value of B 
-- A B A <=> B
-- T T    T
-- T F    F
-- F T    F
-- F F    T

-- Propositions

data Prop = Const Bool
          | Var Char
          | Not Prop
          | And Prop Prop
		  | Or Prop Prop -- Disjunction
		  | Equiv Prop Prop -- Equivalence
          | Imply Prop Prop

p1 :: Prop
p1 = And (Var 'A') (Not (Var 'A'))

p2 :: Prop
p2 = Imply (And (Var 'A') (Var 'B')) (Var 'A')

p3 :: Prop
p3 = Imply (Var 'A') (And (Var 'A') (Var 'B'))

p4 :: Prop
p4 = Imply (And (Var 'A') (Imply (Var 'A') (Var 'B'))) (Var 'B')

p5 :: Prop  -- False when A = F and B = F
p5 = Or (Var 'A') (Var 'B')


p7 :: Prop -- False when A = F, B = T or A = T, B = F 
p7 = Equiv (Var 'A') (Var 'B')

-- Substitutions

type Subst = Assoc Char Bool -- A substitution consists of an Assoc type of a character 'A' and associated boolean (variable and its value)

type Assoc k v = [(k,v)] -- Given a key and value assoc bundles the two into a tuple 

find :: Eq k => k -> Assoc k v -> v -- Given a key and an association determines if key is linked to a value 
find k t = head [v | (k',v) <- t, k == k'] 

-- Tautology checker

eval :: Subst -> Prop -> Bool -- Evaluates a proposition given a substitution 
eval _ (Const b)   = b 
eval s (Var x)     = find x s -- Searches sub for the variable name x and replaces it with the boolean value 
eval s (Not p)     = not (eval s p) -- negate the evaluation of any constation proposition 
eval s (Or p q)    = eval s p || eval s q -- Take the OR of the evaluation of the two propositions 
eval s (Equiv p q) = eval s p == eval s q  -- Take the equality of the evaluation of the two propositions 
eval s (And p q)   = eval s p && eval s q  -- Take the AND of the evaluation of the two propositions 
eval s (Imply p q) = eval s p <= eval s q  -- Take the <= of the evaluation of the two propositions 

vars :: Prop -> [Char] -- Extracts all variables from a proposition 
vars (Const _)   = []
vars (Var x)     = [x] -- Return variable name 
vars (Not p)     = vars p 
vars (Or p q)    = vars p ++ vars q -- Or works same as and/imply, extract vars from L/R props
vars (Equiv p q) = vars p ++ vars q -- Same for equivalence
vars (And p q)   = vars p ++ vars q
vars (Imply p q) = vars p ++ vars q

bools :: Int -> [[Bool]] -- Given an integer which represents no. variables per combination calculate all possible permuatation of True / False 
bools 0 = [[]]           -- E.g. bools 1 = [[False], [True]], bools 2 = [[False,False],[False,True],[True,False],[True,True]]
bools n = map (False:) bss ++ map (True:) bss 
          where bss = bools (n-1)

rmdups :: Eq a => [a] -> [a] -- Removes duplicates from a given list 
rmdups []     = []
rmdups (x:xs) = x : filter (/= x) (rmdups xs)

substs :: Prop -> [Subst] -- Determines all possible variable combinations
substs p = map (zip vs) (bools (length vs)) 
           where vs = rmdups (vars p)  -- Retrieves variables from prop, removes duplicates 

isTaut :: Prop -> Bool -- Given a proposition determine if it's always true 
isTaut p = and [eval s p | s <- substs p] -- eval the proposition with each possible set of var subs, and result to determine if tautology 
