----------------
-- Casey Miles
-- Dec. 12, 2014
----------------

{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Maybe
import Text.Read -- for readMaybe function
import System.IO -- for hFlush function
import System.Random
import Control.Monad (replicateM)
import Graphics.Blank

-- Represent polynomials as tuples
-- of coefficients and exponents
type PolyTerm = (Float, Int)

-- Expressions are <= or >= with
-- a polynomial
data Expr = GThan [PolyTerm] | LThan [PolyTerm] deriving Show



-- Prompt for expression input, print out if
-- successfully parsed
main :: IO ()
main = blankCanvas 3000 { middleware = [] } $ \ context -> do
    send context $ do
    setUpCanvas context



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



-- Define the range of points
-- viewable in the graph plot
graphRange :: (Int, Int)
graphRange = (-500, 500)

-- Get a single random int within range
getRandomInRange :: IO Int
getRandomInRange = getStdRandom $ randomR graphRange

-- Define how many points will be plotted on the graph
numberOfPoints = 10::Int

-- Zip lists of random Ints to make random coordinates
getCoords :: IO [(Int, Int)]
getCoords = do
    -- replicateM :: Monad m => Int -> m a -> m [a]
    -- repeats a monadic action n times, creates list
    xs <- replicateM numberOfPoints getRandomInRange    -- xs::[Int] 
    ys <- replicateM numberOfPoints getRandomInRange    -- ys::[Int] 
    return $ zip xs ys



setUpCanvas :: DeviceContext -> Canvas ()
setUpCanvas context = do
    translate ( width context / 2, height context / 2 )
    scale (1, -1) -- invert y-scale so canvas behaves like Cartesian plot
    rect(-300, -300, 600, 600)
    lineWidth 5
    strokeStyle "#333333"
    stroke()

