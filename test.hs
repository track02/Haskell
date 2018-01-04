-- New functions can be defined in a script file (.hs)
-- Load script functions, GHCi <script>.hs
-- or :load <name> (:l)
-- Scripts can be reloaded using :reload in GHCi

-- Defined functions must begin with a lower-case letter
double x = x + x
quadruple x = double (double x)
factorial n = product [1..n]

{- 
  Alternative to div (sum ns) (length ns)
  A function that takes two arguments can be written in between those arguments in single quotation marks (` not ')
  By convention, list arguments are suffixed with an 's', a list of lists would be 'ss'
-}
average ns = sum ns `div` length ns

