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


main :: IO ()
main = do
    randCoords <- getImpureCoords                           -- generate list of random coordinates
    let pureCoords = getPureCoords                          -- list of pure pseudo-random coords generated using seed

    p1MVar <- newEmptyMVar
    p2MVar <- newEmptyMVar

    blankCanvas 3000 { middleware = [] } $ \ context -> do
        send context $ do
            translate (width context/2, height context/2)   -- center plot on screen
            scale (1, -1)                                   -- invert y-scale so canvas
                                                            -- behaves like Cartesian plot
            drawGraphBackground
            drawPlayerLegend
            plotPoints randCoords "black"
            drawGraphBorder


        forkIO (playerActionLoop "PLAYER 1" p1MVar)         -- prompt p1 for expr
        p1expr <- takeMVar p1MVar                           -- blocks until p1 has expr'd
        let p1 = Player p1expr strokeColor1 fillColor1


        forkIO (playerActionLoop "PLAYER 2" p2MVar)         -- prompt for p2's expr
        p2expr <- takeMVar p2MVar
        let p2 = Player p2expr strokeColor2 fillColor2


        send context $ do
            displayPlayerMove p1                            -- Display both player's exprs
            displayPlayerMove p2
            let colors = map colorForPoint [ (didHit p1 coord, didHit p2 coord) | coord <- randCoords ]
            sequence_ $ zipWith plotPoint colors randCoords -- redraw color-coded points
            drawGraphBorder



----------------------------------------------------------------
-- PLAYER INPUT
----------------------------------------------------------------

-- Prompt player to enter an expression.
playerActionLoop :: String -> MVar Expr -> IO ()
playerActionLoop name var = do
    putStrLn $ name ++ " ------------------------------------------------"
    input <- promptForExpr
    case parseExpr input of
        Just expr -> do
            putMVar var expr
        Nothing -> do
            putStrLn    ("Could not parse your input.\n" ++
                        "ex: \"y <= [(0.0002,3),(2,1)]\"")
            playerActionLoop name var


-- From HaskellWiki: Intro to Haskell IO Actions
-- https://www.haskell.org/haskellwiki/Introduction_to_Haskell_IO/Actions
promptLine :: String -> IO String
promptLine prompt = putStr prompt >> hFlush stdout >> getLine

-- Specialize prompt for equation input
promptForExpr :: IO String
promptForExpr = 
    promptLine "Enter comparison operator and [(Coefficient, Exponent)] values\ny "


-- Parse user's input. Fairly brittle,
-- expects "<= [PolyTerm]" input
parseExpr :: String -> Maybe Expr
parseExpr (a:b:terms) = case readTermsMaybe terms of
    Just (t:ts) -> if (a:b:[]) == "<=" 
        then Just $ LThan (t:ts)
        else Just $ GThan (t:ts)
    Just [] -> Nothing
    Nothing -> Nothing


readTermsMaybe :: String -> Maybe [PolyTerm]
readTermsMaybe str = readMaybe str


----------------------------------------------------------------
-- COORDINATE GENERATION
----------------------------------------------------------------

-- Get list of values from seeded generator
getPureRandoms :: StdGen -> [Double]
getPureRandoms gen =
    take numberOfPoints $ randomRs graphRange gen

getPureCoords =
    let g           = mkStdGen seed                     -- use defined seed to create a generator
        (ga, gb)    = split g                           -- create 2nd generator
        xs          = getPureRandoms ga
        ys          = getPureRandoms gb
    in zip xs ys


-- Get random int (within range) using system's standard generator
getRandomInRange :: IO Double
getRandomInRange = getStdRandom $ randomR graphRange


getImpureCoords :: IO [(Double, Double)]
getImpureCoords = do
    -- replicateM repeats a monadic action n times
    xs <- replicateM numberOfPoints getRandomInRange    -- xs::[Double]
    ys <- replicateM numberOfPoints getRandomInRange    -- ys::[Double]
    return $ zip xs ys


----------------------------------------------------------------
-- DISPLAY PLAYER ACTIONS
----------------------------------------------------------------

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


-- Plots player's move input and fills in plot,
-- depending on whether the given expression is
-- (y <= ...) or (y >= ...)
displayPlayerMove :: Player -> Canvas ()
displayPlayerMove (Player expr sc fc) = do
    let fn = evalExpr expr                      -- generate (dbl -> dbl) function
    beginPath()
    plotExpr fn                                 -- plot function line
    case expr of
        GThan terms -> do lineTo (300,300)      -- fill above the line
                          lineTo (-300,300)
                          fillStyle fc
                          fill()
        LThan terms -> do lineTo (300,-300)     -- fill below the line
                          lineTo (-300,-300)
                          fillStyle fc
                          fill()
    lineWidth 4
    strokeStyle sc
    stroke()
    closePath()



-- Create a function from x -> y
-- From a list of PolyTerms
evalExpr :: Expr -> (Double -> Double)
evalExpr (LThan terms) = evalExpr (GThan terms)
evalExpr (GThan terms) =
    (\x -> sum $ map (\f -> f x) evalTerms)
        where evalTerms = [ (\x -> c*(x^e)) | (c,e) <- terms ]  -- c=coeff, e=expo

-- Plot line of player's expression
plotExpr :: (Double -> Double) -> Canvas ()
plotExpr f = do
    let range = [(fst graphRange)..(snd graphRange)]
    let coords = [ (x, (boundToGraph $ f x)) | x <- range ]
    moveTo (coords !! 0)
    sequence_ $ map lineTo coords


-- Keep y-values within range of graph
boundToGraph :: (Double -> Double)
boundToGraph = bounded graphRange

bounded :: (Double, Double) -> (Double -> Double)
bounded (low, high) = (\val -> min high $ max low val)


-- Determine if a coordinate is within the
-- filled area of a player's expression
didHit :: Player -> (Double, Double) -> Bool
didHit (Player e _ _) (x,y) = compare' y playery where
    compare' = case e of
        GThan _ -> (>=)
        LThan _ -> (<=)
    fn = evalExpr e
    playery = fn x


colorForPoint :: (Bool, Bool) -> Color
colorForPoint (False, False) = "black"
colorForPoint (False, True ) = strokeColor2     -- only player2 hit
colorForPoint (True,  False) = strokeColor1     -- only player1 hit
colorForPoint (True,  True ) = strokeColor3     -- both players hit,
                                                -- display a 3rd color


----------------------------------------------------------------
-- DRAWING BACKGROUND
----------------------------------------------------------------

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


drawPlayerLegend :: Canvas ()
drawPlayerLegend = do
    save()
    scale(1,-1)

    plotPoint strokeColor1 (350,-275)
    plotPoint strokeColor2 (350,-250)
    plotPoint strokeColor3 (350,-225)

    font "10pt Consolas"
    fillStyle "black"
    fillText("Player 1", 360, -271)
    fillText("Player 2", 360, -246)
    fillText("Both players", 360, -221)
    restore()


----------------------------------------------------------------
-- TYPES
----------------------------------------------------------------
type Color = Text

-- Represent polynomials as tuples
-- of coefficients and exponents
type PolyTerm = (Double, Int)

-- Expressions are <= or >= with a polynomial
data Expr = GThan [PolyTerm] | LThan [PolyTerm] deriving (Show, Eq, Ord)

-- Players associated with an expression and some colors
data Player = Player Expr Color Color deriving (Show, Eq, Ord)

----------------------------------------------------------------
-- SOME SETTINGS
----------------------------------------------------------------

-- Set point generation mode.
-- False will generate coords with seed below
useRandomCoords = True

-- Define a seed for generation of
-- pseudo-random (x,y) coordinates
seed = 8::Int

-- Color values for strokes and fills
strokeColor1 = "rgba(255,215,0,1)"
fillColor1   = "rgba(255,215,0,0.1)"
strokeColor2 = "rgba(32,178,70,1)"
fillColor2   = "rgba(32,178,70,0.1)"
strokeColor3 = "rgba(178,32,40,1)"
fillColor3   = "rgba(178,32,40,0.1)"

-- Define how many points will be plotted on the graph
numberOfPoints = 15::Int

-- Define the range of points in graph plot
graphRange = (-300, 300)::(Double,Double)

