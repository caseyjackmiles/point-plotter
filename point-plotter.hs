----------------
-- Casey Miles
-- Dec. 12, 2014
----------------

module Main where

import Data.Maybe
import Text.Read -- for readMaybe function
import System.IO -- for hFlush function

-- Represent polynomials as tuples
-- of coefficients and exponents
type PolyTerm = (Float, Int)

-- Expressions are <= or >= with
-- a polynomial
data Expr = GThan [PolyTerm] | LThan [PolyTerm] deriving Show



-- Prompt for expression input, print out if
-- successfully parsed
main :: IO ()
main = do
    inp <- promptForExpr
    case parseExpr inp of
        Nothing -> print "NOTHING!"
        Just e -> print e


-- From HaskellWiki: Intro to Haskell IO Actions
promptLine :: String -> IO String
promptLine prompt = putStr prompt >> hFlush stdout >> getLine

-- Specialize prompt for equation input
promptForExpr :: IO String
promptForExpr = 
    promptLine "\nEnter comparison operator and [(Coefficient, Exponent)] values\ny "


-- Easy parsing of expression
parseExpr :: String -> Maybe Expr
parseExpr (a:b:terms) = case readTermsMaybe terms of
    Just (t:ts) -> if (a:b:[]) == "<=" 
        then Just $ LThan (t:ts)
        else Just $ GThan (t:ts)
    Just [] -> Nothing
    Nothing -> Nothing


readTermsMaybe :: String -> Maybe [PolyTerm]
readTermsMaybe str = readMaybe str

