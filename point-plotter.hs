----------------
-- Casey Miles
-- Dec. 12, 2014
----------------


-- Represent polynomials as tuples
-- of coefficients and exponents
type PolyTerm = (Float, Int)

-- Expressions are <= or >= with
-- a polynomial
data Expr = GThan [PolyTerm] | LThan [PolyTerm] deriving Show


-- Do nothing for now
main :: IO ()
main = return ()

