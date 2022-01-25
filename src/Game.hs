--------------------------------------------------------------------------------
-- Functional Programming - Super Functor Factory Project
--------------------------------------------------------------------------------
-- Copyright (c) 2022 Michael B. Gale (michael@fpclass.online)
--
-- This source code is subject to the terms and conditions found in the LICENSE
-- file in the root directory of this source tree.
--------------------------------------------------------------------------------

module Game where

--------------------------------------------------------------------------------

import Data.List
import Data.Function

--------------------------------------------------------------------------------

-- | This type enumerates the different colours of lambdas that arrive at
-- the Functor Factory.
data Colour = Purple | Red | Green | Blue | Yellow
    deriving (Eq, Show, Enum, Bounded)

-- | Cells are either empty or contain a lambda of some colour.
data Cell = Empty | Lambda Colour
    deriving (Eq, Show)

-- | The conveyor belt is represented as a list of lists of cells.
type Conveyor = [[Cell]]

-- | Represents a position on the conveyor belt where the first component is
-- the coordinate on the x-axis and the second component is the coordinate
-- on the y-axis.
type Pos = (Int, Int)

-- | `conveyorWidth` is the width of the conveyor belt.
conveyorWidth :: Int
conveyorWidth = 10

-- | `conveyorHeight` is the height of the conveyor belt.
conveyorHeight :: Int
conveyorHeight = 15

-- | `lambdaLimit` is the maximum number of lambdas that will arrive at the
-- factory after the initial delivery.
lambdaLimit :: Int
lambdaLimit = 100

--------------------------------------------------------------------------------

-- | `profit` @positions@ calculates the profit for @positions@-many lambdas.
profit :: [Pos] -> Int
profit = undefined

-- | `isOutsideBounds` @pos@ determines whether @pos@ is outside the boundaries
-- of the conveyor belt and evaluates to `True` if that is the case or `False`
-- otherwise.
isOutsideBounds :: Pos -> Bool
isOutsideBounds = undefined

-- | `isEmpty` @conveyor@ determines whether all cells on the @conveyor@ are
-- empty or not.
isEmpty :: Conveyor -> Bool
isEmpty = undefined

-- | `makeEmpty` @conveyor positions@ should turn all cells indicated by the
-- given @positions@ to empty cells.
makeEmpty :: Conveyor -> [Pos] -> Conveyor
makeEmpty = undefined

-- | `advanceConveyor` @conveyor@ should move all lambdas on @conveyor@ as
-- close to the end of the conveyor as possible.
advanceConveyor :: Conveyor -> Conveyor
advanceConveyor = undefined

-- | `fill` @conveyor lambdas@ should add new @lambdas@ the @conveyor@.
fill :: Conveyor -> [Cell] -> Conveyor
fill = undefined

-- | `nextMove` @conveyor@ should determine the next cluster on @conveyor@ to
-- process with the goal of maximising the Functor Factory's profit.
nextMove :: Conveyor -> [Pos]
nextMove = undefined

--------------------------------------------------------------------------------