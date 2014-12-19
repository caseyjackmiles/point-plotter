----------------
-- Casey Miles
-- Dec. 12, 2014
----------------

{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Maybe
import Text.Read -- for readMaybe function
import System.IO -- for hFlush function
import Data.Text(Text)
import System.Random
import Control.Monad (replicateM)
import Control.Concurrent
import qualified Data.Map as Map
import Graphics.Blank

-- Color values for strokes and fills
strokeColor1 = "rgba(255,215,0,1)"
fillColor1   = "rgba(255,215,0,0.1)"
strokeColor2 = "rgba(32,178,70,1)"
fillColor2   = "rgba(32,178,70,0.1)"
strokeColor3 = "rgba(178,32,40,1)"
fillColor3   = "rgba(178,32,40,0.1)"


-- Define a seed for generation of
-- pseudo-random (x,y) coordinates
seed = 8::Int

-- Define how many points will be plotted on the graph
numberOfPoints = 10::Int

-- Represent polynomials as tuples
-- of coefficients and exponents
type PolyTerm = (Double, Int)

-- Expressions are <= or >= with
-- a polynomial
data Expr = GThan [PolyTerm] | LThan [PolyTerm] deriving (Show, Eq, Ord)

type Color = Text

-- Players associated with an expression and some colors
data Player = Player Expr Color Color deriving (Show, Eq, Ord)




-- Prompt for expression input, print out if
-- successfully parsed
main :: IO ()
main = do
    randCoords <- getCoords      -- random generation
    let coords = getPureCoords  -- deterministic, with seed

    p1MVar <- newEmptyMVar
    p2MVar <- newEmptyMVar

    -- Fork a thread for user to be able to input expr
    forkIO (playerActionLoop "PLAYER 1" p1MVar)
    --forkIO (playerActionLoop "PLAYER 2" p2MVar)

    ------------------------------------------------------
    blankCanvas 3000 { middleware = [] } $ \ context -> do
        send context $ do
            translate (width context/2, height context/2)   -- center plot on screen
            scale (1, -1)                                   -- invert y-scale so canvas
                                                            -- behaves like Cartesian plot
            drawGraphBackground
            plotPoints randCoords "black"
            drawGraphBorder

        p1expr <- takeMVar p1MVar                       -- blocks until p1 has expr'd
        let player1 = Player p1expr strokeColor1 fillColor1

        forkIO (playerActionLoop "PLAYER 2" p2MVar)

        p2expr <- takeMVar p2MVar
        let player2 = Player p2expr strokeColor2 fillColor2

        send context $ do
            displayPlayerMove player1
            displayPlayerMove player2
            let colors = map colorForPoint [ (didHit player1 coord, didHit player2 coord) | coord <- randCoords ]
            sequence_ $ zipWith plotPoint colors randCoords
            drawGraphBorder


playerActionLoop :: String -> MVar Expr -> IO ()
playerActionLoop name var = do
    putStrLn $ name ++ " ------------------------------------------------"
    input <- promptForExpr
    case parseExpr input of
        Just expr -> do
            putStrLn "Successful parse"
            putMVar var expr
        Nothing -> do
            putStrLn "Unsuccessful parse\n"
            playerActionLoop name var


-- From HaskellWiki: Intro to Haskell IO Actions
promptLine :: String -> IO String
promptLine prompt = putStr prompt >> hFlush stdout >> getLine

-- Specialize prompt for equation input
promptForExpr :: IO String
promptForExpr = 
    promptLine "Enter comparison operator and [(Coefficient, Exponent)] values\ny "


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
graphRange :: (Double, Double)
graphRange = (-300, 300)


getPureRandoms :: StdGen -> [Double]
getPureRandoms gen =
    take numberOfPoints $ randomRs graphRange gen

getPureCoords =
    let g           = mkStdGen seed
        (ga, gb)    = split g
        xs          = getPureRandoms ga
        ys          = getPureRandoms gb
    in zip xs ys





{- ##############################
 - RANDOM, IMPURE IO COORDINATES
 ############################## -}

-- Get a single random int within range
getRandomInRange :: IO Double
getRandomInRange = getStdRandom $ randomR graphRange

-- Zip lists of random Ints to make random coordinates
getCoords :: IO [(Double, Double)]
getCoords = do
    -- replicateM :: Monad m => Int -> m a -> m [a]
    -- repeats a monadic action n times, creates list
    xs <- replicateM numberOfPoints getRandomInRange    -- xs::[Double]
    ys <- replicateM numberOfPoints getRandomInRange    -- ys::[Double]
    return $ zip xs ys

{- ######################### -}





plotPoints :: [(Double,Double)] -> Color -> Canvas ()
plotPoints pts col = do
    sequence_ $ fmap (plotPoint col) pts


plotPoint :: Color -> (Double, Double) -> Canvas ()
plotPoint col (x,y) = do
    beginPath()
    arc(x, y, 1, 0, 2*pi, False)
    lineWidth 5
    strokeStyle col
    fillStyle col
    fill()
    stroke()
    closePath()



drawGraphBorder :: Canvas ()
drawGraphBorder = do
    beginPath()
    rect(-300, -300, 600, 600)
    lineWidth 5
    lineJoin "miter"
    strokeStyle "#333333"
    stroke()

drawVerticalLines :: Canvas ()
drawVerticalLines = do
    let coordPairs = [ (x,y) | x <- [-250,-200..250], y <- [300,-300] ]
    -- Build list of canvas commands
    let commands = zipWith (\ cmd coord -> cmd coord) (cycle [moveTo, lineTo]) coordPairs
    -- Now perform the actions
    sequence_ commands
    lineWidth 0.5
    strokeStyle "#bdbdbd"
    stroke()

drawHorizontalLines :: Canvas ()
drawHorizontalLines = do
    let coordPairs = [ (x, y) | y <- [-250,-200..250], x <- [300, -300] ]
    let commands = zipWith (\ cmd coord -> cmd coord) (cycle [moveTo, lineTo]) coordPairs
    sequence_ commands
    lineWidth 0.5
    strokeStyle "#bdbd99"
    stroke()

drawGraphBackground = drawVerticalLines >> drawHorizontalLines





-- Create a function from x -> y
evalExpr :: Expr -> (Double -> Double)
evalExpr (GThan terms) =
    (\x -> sum $ map (\f -> f x) evalTerms)
        where evalTerms = [ (\x -> c*(x^e)) | (c,e) <- terms ]  -- c=coeff, e=expo
evalExpr (LThan terms) = evalExpr (GThan terms)

plotExpr :: (Double -> Double) -> Canvas ()
plotExpr f = do
    let range = [(fst graphRange)..(snd graphRange)]
    let coords = [ (x, (boundToGraph $ f x)) | x <- range ]
    moveTo (coords !! 0)
    sequence_ $ map lineTo coords


displayPlayerMove :: Player -> Canvas ()
displayPlayerMove (Player expr sc fc) = do
    let fn = evalExpr expr
    beginPath()
    plotExpr fn -- plot actual function
    case expr of
        GThan terms -> do lineTo (300,300)
                          lineTo (-300,300)
                          fillStyle fc
                          fill()
        LThan terms -> do lineTo (300,-300)
                          lineTo (-300,-300)
                          fillStyle fc
                          fill()
    lineWidth 4
    strokeStyle sc
    stroke()
    closePath()



bounded :: (Double, Double) -> (Double -> Double)
bounded (low, high) = (\val -> min high $ max low val)

boundToGraph :: (Double -> Double)
boundToGraph = bounded graphRange


----------------------------------------------------------------

didHit :: Player -> (Double, Double) -> Bool
didHit (Player e _ _) (x,y) = compare' y playery where
    compare' = case e of
        GThan _ -> (>=)
        LThan _ -> (<=)
    fn = evalExpr e
    playery = fn x


colorForPoint :: (Bool, Bool) -> Color
colorForPoint (False, False) = "black"
colorForPoint (False, True ) = strokeColor2
colorForPoint (True,  False) = strokeColor1
colorForPoint (True,  True ) = strokeColor3

