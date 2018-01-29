-- Extend the abstract machine to support multiplication

data Expr = Val Int | Add Expr Expr | Mult Expr Expr -- Add a new multiplication expression type 

type Cont = [Op] -- Control Stack, holds operations

data Op = EVAL_ADD Expr | EVAL_MULT Expr | ADD Int | MULT Int 

eval :: Expr -> Cont -> Int  -- Evaluates an expression and updates the stack
eval (Val n)    c = exec c n  -- If value expression is reached then there's nothing else to evaluate - start working back down stack executing the stored up commands 
eval (Add x y)  c = eval x (EVAL_ADD y  : c) -- If Add expression reached, split into two evaluations for LHS and RHS, 
eval (Mult x y) c = eval x (EVAL_MULT y : c) -- If Mult expression reached, split into two evaluations for LHS and RHS, a              


exec :: Cont -> Int -> Int  -- Pops the topmost command from the stack and executes it 
exec []           n = n     -- No command - return the stored value 
exec (EVAL_ADD  y : c) n = eval y (ADD  n : c) -- Add Eval command - branch of an Add expression, evaluate the expression y storing current "position" in an add command 
exec (EVAL_MULT y : c) n = eval y (MULT n : c) -- Mult Eval command - branch of a mult expression, evaluate the expression y storing current "position" in a mult command 
exec (ADD  n : c) m = exec c (n+m) -- Add command, add value to register value and move to next command 
exec (MULT n : c) m = exec c (n*m) -- Mult command, multiply value with register value and move on to next command 

value :: Expr -> Int
value e = eval e []



-- e1 = Add (Val 5) (Val 6)
-- value e1                     -----> eval Add (Val 5) (Val 6) [] 
-- eval Add (Val 5) (Val 6) []  -----> eval (Val 5) [EVAL (Val 6)]    ***Evaluating LHS, store evaluation of RHS 
-- eval (Val 5) [EVAL (Val 6)]  -----> exec [EVAL (Val 6)] 5          ***Value expression reached, tell stack to execute commands using the retrieved value 5 
-- exec [EVAL (Val 6)] 5        -----> eval (Val 6) [ADD 5]           ***Stack sees there's still an expression to evaluate, store LHS Value in an ADD command 
-- eval (Val 6) [ADD 5]         -----> exec [ADD 5] 6                 ***Expression evaluated, tell stack to execute commands using the retrieved value 6
-- exec [ADD 5] 6               -----> exec [] (5 + 6)                ***Stack sees there's a pending add command, pop it and execute again with result (5 + 6)
-- exec [] 11                   -----> 11                             ***There are no further commands to be executed using the given value, so return it 
--                                                                    ***Otherwise this  value would be stored in an ADD command and  the process would repeat
--                                                                    ***in evaluating the RHS of the enclosing Add expression Add (Add (Val 5) (Val 6)) (Val 1)
--