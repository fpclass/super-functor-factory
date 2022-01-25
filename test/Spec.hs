--------------------------------------------------------------------------------
-- Functional Programming - Super Functor Factory Project
--------------------------------------------------------------------------------
-- Copyright (c) 2022 Michael B. Gale (michael@fpclass.online)
--
-- This source code is subject to the terms and conditions found in the LICENSE
-- file in the root directory of this source tree.
--------------------------------------------------------------------------------

{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

--------------------------------------------------------------------------------

import Control.Monad

import Data.List (notElem)

import Test.Tasty
import Test.Tasty.Hedgehog
import Test.Tasty.HUnit as Unit
import Test.Tasty.Ingredients
import Test.Tasty.Ingredients.Basic
import Test.Tasty.Runners.AntXML

import Hedgehog as H
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range

import Game as G

--------------------------------------------------------------------------------

colour :: MonadGen m => m Colour
colour = Gen.element [minBound..maxBound]

cell :: MonadGen m => m Cell
cell = Gen.frequency [(1, pure Empty), (3, Lambda <$> colour)]

filledCell :: MonadGen m => m Cell
filledCell = Lambda <$> colour

conveyorOfSize :: MonadGen m => Int -> Int -> m Conveyor
conveyorOfSize w h = replicateM w (replicateM h cell)

conveyor :: MonadGen m => m Conveyor
conveyor = conveyorOfSize G.conveyorWidth G.conveyorHeight

pos :: MonadGen m => m Pos
pos = do
    x <- Gen.element [0..G.conveyorWidth-1]
    y <- Gen.element [0..G.conveyorHeight-1]
    pure (x,y)

--------------------------------------------------------------------------------

-- | `checkDimensions` @conveyor0 conveyor1@ checks that the dimensions of the
-- two given conveyor belts are the same.
checkDimensions :: (HasCallStack, MonadTest m) => Conveyor -> Conveyor -> m ()
checkDimensions c c' = do
    length c === length c'

    forM_ (zip c c') $ \(col0, col1) ->
        length col0 === length col1

-- | `checkEmpty` @positions conveyor0 conveyor1@ checks that all cells
-- indicated by @positions@ are empty in @conveyor1@ and that all other cells
-- in @conveyor1@ match the respective ones in @conveyor0@.
checkEmpty :: [Pos] -> Conveyor -> Conveyor -> Bool
checkEmpty ps c c' =
    length c == length c' &&
    and [go (ix,0) r r' | (ix,(r,r')) <- zip [0..] (zip c c')]
    where go _     []    []     = True
          go _     []    _      = False
          go _     _     []     = False
          go (x,y) (c:r) (c':r')
            | (x,y) `elem` ps = c' == G.Empty && go (x,y+1) r r'
            | otherwise = c==c' && go (x,y+1) r r'

-- | `checkAdvanced` @conveyor@ checks that all empty cells in @conveyor@
-- appear exclusively at the top of columns.
checkAdvanced :: (HasCallStack, MonadTest m) => Conveyor -> m ()
checkAdvanced = mapM_ $ \column -> do
    let withoutEmptyCells = dropWhile (==Empty) column
    annotateShow withoutEmptyCells

    H.assert $ notElem Empty withoutEmptyCells

-- | `areAdjacent` @p0 p1@ checks whether @p0@ and @p1@ are adjacent to each
-- other.
areAdjacent :: Pos -> Pos -> Bool
areAdjacent (x0,y0) (x1,y1) =
       (x0 == x1 && (y0-1 == y1 || y0+1 == y1))
    || (y0 == y1 && (x0-1 == x1 || x0+1 == x1))

-- | `isCluster` @positions@ determines whether the cells at @positions@ form a
-- cluster with each other. A cluster cannot be empty.
isCluster :: (HasCallStack, MonadTest m) => [Pos] -> m ()
isCluster [] = failure
isCluster ps = go ps []
    where go []     rs  = success
          go (x:xs) rs = do
              -- `x` must be adjacent to some other position in the input
              H.assert $ any (areAdjacent x) (xs ++ rs)

              -- add `x` to the list of positions we have checked and
              -- continue with `xs`
              go xs (x:rs)

-- | `countEmpties` @conveyor@ counts how many empty cells there are
-- in @conveyor@.
countEmpties :: Conveyor -> Int
countEmpties = sum . map (length . filter (==Empty))

--------------------------------------------------------------------------------

prop_correctProfit :: Property
prop_correctProfit = property $ do
    ps <- forAll $ Gen.list (Range.constant 1 100) pos
    length ps === round (sqrt (fromIntegral (G.profit ps)))

profitTests :: TestTree
profitTests = testGroup "profit"
    [ testProperty
        "is the number of lambdas raised to 2"
        "prop_correctProfit"
        prop_correctProfit
    ]

--------------------------------------------------------------------------------

prop_insideBounds :: Property
prop_insideBounds = property $ do
    x <- forAll $ Gen.int (Range.constant 0 (G.conveyorWidth-1))
    y <- forAll $ Gen.int (Range.constant 0 (G.conveyorHeight-1))
    H.assert $ not $ G.isOutsideBounds (x,y)

prop_outsideBounds :: Property
prop_outsideBounds = property $ do
    x <- forAll $ Gen.choice [
        Gen.int (Range.constant minBound 0)
     ,  Gen.int (Range.constant G.conveyorWidth maxBound)
     ]
    y <- forAll $ Gen.choice [
        Gen.int (Range.constant minBound 0)
     ,  Gen.int (Range.constant G.conveyorHeight maxBound)
     ]
    H.assert $ G.isOutsideBounds (x,y)

isOutsideBoundsTests :: TestTree
isOutsideBoundsTests = testGroup "isOutsideBounds"
    [ testProperty
        "returns False for positions in bounds"
        "prop_insideBounds"
        prop_insideBounds
    , testProperty
        "returns True for positions out of bounds"
        "prop_outsideBounds"
        prop_outsideBounds
    ]

--------------------------------------------------------------------------------

prop_emptyConveyor :: Property
prop_emptyConveyor = property $ do
    w <- forAll $ Gen.int (Range.constant 0 100)
    h <- forAll $ Gen.int (Range.constant 0 100)
    let c = replicate w $ replicate h G.Empty
    H.assert $ G.isEmpty c

prop_nonEmptyConveyor :: Property
prop_nonEmptyConveyor = property $ do
    c <- forAll conveyor
    when (all (all (==G.Empty)) c) discard
    H.assert $ not $ G.isEmpty c

isEmptyTests :: TestTree
isEmptyTests = testGroup "isEmptyTests"
    [ testProperty
        "returns True for empty conveyor belts"
        "prop_emptyConveyor"
        prop_emptyConveyor
    , testProperty
        "returns False for non-empty conveyor belts"
        "prop_nonEmptyConveyor"
        prop_nonEmptyConveyor
    ]

--------------------------------------------------------------------------------

prop_emptiesCells :: Property
prop_emptiesCells = property $ do
    c <- forAll conveyor
    pos <- forAll $
        Gen.list (Range.constant 0 (G.conveyorWidth*G.conveyorHeight)) pos
    H.assert $ checkEmpty pos c $ G.makeEmpty c pos

makeEmptyTests :: TestTree
makeEmptyTests = testGroup "makeEmpty"
    [ testProperty
        "sets specified cells to Empty"
        "prop_emptiesCells"
        prop_emptiesCells
    ]

--------------------------------------------------------------------------------

prop_maintainsDimensions :: Property
prop_maintainsDimensions = property $ do
    c <- forAll conveyor
    checkDimensions c (G.advanceConveyor c)

prop_emptyCellsOnTop :: Property
prop_emptyCellsOnTop = property $ do
    c <- forAll conveyor
    checkAdvanced (G.advanceConveyor c)

advanceConveyorTests :: TestTree
advanceConveyorTests = testGroup "advanceConveyor"
    [ testProperty
        "does not change the dimensions of the conveyor"
        "prop_maintainsDimensions"
        prop_maintainsDimensions
    , testProperty
        "Empty cells are only at the top of each column"
        "prop_emptyCellsOnTop"
        prop_emptyCellsOnTop
    ]

--------------------------------------------------------------------------------

prop_fillsEmptySpaces :: Property
prop_fillsEmptySpaces = property $ do
    c <- forAll conveyor
    cs <- forAll $ Gen.list (Range.constant 1 20) filledCell
    let empty  = countEmpties c
        result = G.fill (G.advanceConveyor c) cs
    result === G.advanceConveyor result
    countEmpties result === empty-length cs

fillTests :: TestTree
fillTests = testGroup "fill"
    [ testProperty
        "fills empty spaces"
        "prop_fillsEmptySpaces"
        prop_fillsEmptySpaces
    ]

--------------------------------------------------------------------------------

prop_adjacentPositions :: Property
prop_adjacentPositions = property $ do
    c <- forAll conveyor
    isCluster (G.nextMove c)

nextMoveTests :: TestTree
nextMoveTests = testGroup "nextMove"
    [ testProperty
        "positions are adjacent to each other"
        "prop_adjacentPositions"
        prop_adjacentPositions
    ]

--------------------------------------------------------------------------------

-- | `tests` is the main `TestTree` for the Super Functor Factory test suite.
tests :: TestTree
tests = localOption (HedgehogShowReplay True)
     $ testGroup "Game"
     [ profitTests
     , isOutsideBoundsTests
     , isEmptyTests
     , makeEmptyTests
     , advanceConveyorTests
     , fillTests
     , after AllSucceed "isOutsideBounds" nextMoveTests
     ]

-- | The list of tasty ingredients. Note: the order seems to matter,
-- `antXMLRunner` won't work at all if placed last in the list.
ingredients :: [Ingredient]
ingredients = [antXMLRunner, listingTests, consoleTestReporter]

-- | `main` is the main entry point to the test suite.
main :: IO ()
main = defaultMainWithIngredients ingredients tests

--------------------------------------------------------------------------------
