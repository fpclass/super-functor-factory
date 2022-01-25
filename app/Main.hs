--------------------------------------------------------------------------------
-- Functional Programming - Super Functor Factory Project
--------------------------------------------------------------------------------
-- Copyright (c) 2022 Michael B. Gale (michael@fpclass.online)
--
-- This source code is subject to the terms and conditions found in the LICENSE
-- file in the root directory of this source tree.
--------------------------------------------------------------------------------

module Main where

--------------------------------------------------------------------------------

import Control.Monad

import Data.Hashable
import Data.List (transpose)

import qualified System.Console.ANSI as C
import System.IO
import System.Random

import GHC.IO.Encoding

import Game
import Config

--------------------------------------------------------------------------------
-- Rendering
--------------------------------------------------------------------------------

-- | `sgrForColour` @colour@ determines how to render @colour@ in a terminal.
sgrForColour :: Colour -> [C.SGR]
sgrForColour Purple = [C.SetColor C.Foreground C.Vivid C.Magenta]
sgrForColour Red    = [C.SetColor C.Foreground C.Vivid C.Red]
sgrForColour Green  = [C.SetColor C.Foreground C.Dull C.Green]
sgrForColour Blue   = [C.SetColor C.Foreground C.Dull C.Blue]
sgrForColour Yellow = [C.SetColor C.Foreground C.Dull C.Yellow]

-- | `charForColour` @colour@ maps @colour@ to a character representation.
charForColour :: Colour -> Char
charForColour Purple = 'P'
charForColour Red    = 'R'
charForColour Green  = 'G'
charForColour Blue   = 'B'
charForColour Yellow = 'Y'

-- | `renderCell` @cell@ renders @cell@ to the standard output.
renderCell :: Config -> Cell -> IO ()
renderCell _   Empty      = putChar ' '
renderCell cfg (Lambda c) = do
    unless (cfgNoColour cfg) (C.setSGR (sgrForColour c))
    putChar $ if cfgNoColour cfg || cfgNoUnicode cfg
              then charForColour c else 'λ'
    C.setSGR [C.Reset]

-- | `renderRow` @labelColumnSize row@ renders @row@ (comprised of a
-- label and the row's cells). @labelColumnSize@ is the size of the
-- largest row label (to determine spacing).
renderRow :: Config -> Int -> (String,[Cell]) -> IO ()
renderRow cfg ll (label,row) = do
    putStr label
    putStr (replicate (ll - length label + 1) ' ')
    mapM_ (renderCell cfg) row
    putStrLn ""

-- | `renderConveyor` @conveyor@ renders @conveyor@ to the standard output.
renderConveyor :: Config -> Conveyor -> IO ()
renderConveyor cfg conveyor = do
    let ls = map show [0..conveyorHeight-1]
        ll = maximum (map length ls)
    putStr (replicate (ll+1) ' ')
    forM_ [0..conveyorWidth-1] (putStr . show)
    putStrLn ""
    mapM_ (renderRow cfg ll) (zip ls (transpose conveyor))

--------------------------------------------------------------------------------
-- Randomness
--------------------------------------------------------------------------------

instance Random Colour where
    randomR (l,h) gen = (toEnum r, g')
        where (r, g') = randomR (fromEnum l, fromEnum h) gen

    random = randomR (minBound,maxBound)

-- | `randomCell` is a computation which generates a random `Cell` value. The
-- value will always be a `Lambda` of a random colour, never an empty cell.
randomCell :: IO Cell
randomCell = Lambda <$> randomIO

-- | `randomBoard` @width height@ generates a random conveyor with the specified
-- dimensions.
randomBoard :: Int -> Int -> IO Conveyor
randomBoard width height = replicateM width (replicateM height randomCell)

--------------------------------------------------------------------------------
-- Game loop
--------------------------------------------------------------------------------

-- | `turn` @config conveyor profit remainingLambdas@ simulates the Functor
-- Factory with @conveyor@. The current @profit@ and the number of
-- @remainingLambdas@ are passed recursively as arguments.
turn :: Config -> Conveyor -> Int -> Int -> IO ()
turn cfg conveyor currentProfit limit
    | isEmpty conveyor = do
        -- print the final profit to the standard output
        putStr "Final profit: £"
        print currentProfit
    | otherwise = do
        -- clear the screen and render the current state of the conveyor
        C.clearScreen
        renderConveyor cfg conveyor
        putStr "Current profit: £"
        print currentProfit
        -- ask the AI for the next move, calculate the profit for it,
        -- and advance the existing lambdas on the conveyor belt
        let cluster = nextMove conveyor
            points  = profit cluster
            emptied = advanceConveyor $ makeEmpty conveyor cluster
        -- create as many new lambdas as possible and fill the conveyor
        -- belt with them
        cs <- replicateM (min (length cluster) limit) randomCell
        let filled = fill emptied cs
        -- wait for the user to press enter before resuming
        putStrLn "Press ENTER to continue."
        getLine
        -- resume with the updated conveyor, profit, and remaining lambdas
        turn cfg filled (currentProfit + points) (max 0 (limit - length cluster))

-- | `game` represents a computation which represents the main game loop.
game :: Config -> IO ()
game cfg = do
    -- prompt the user for a seed for the pseudo-RNG; this allows users to get
    -- the same puzzles given the same seed
    putStr "Enter seed: "
    random <- mkStdGen . hash <$> getLine
    setStdGen random

    -- generate a random conveyor (using the specified seed) and run the game
    -- with that conveyor
    conveyor <- randomBoard conveyorWidth conveyorHeight
    turn cfg conveyor 0 lambdaLimit

    -- start a new game
    game cfg

-- | `main` represents the main entry point for this program.
main :: IO ()
main = do
    cfg <- parseCmdLineArgs
    setLocaleEncoding utf8
    hSetBuffering stdout NoBuffering
    hSetBuffering stdin  NoBuffering
    game cfg

--------------------------------------------------------------------------------
