module Gridlock.GridPatterns ( create2x2, patternPoints ) where

import qualified Data.Map as Map
import Gridlock.GameState ( sumCoords )
import Gridlock.Types
import Control.Monad.State.Lazy
import GHC.Base (ord)

-- For creating the patterns, the only thing we need to check is that they're valid, ie. no adjacent colours
-- checkAdjSquares takes the new potential cell and grid, and returns True if valid, False otherwise 
allowCell :: Cell -> Coord -> Map.Map Coord Cell -> Bool
allowCell c coo grid = Just c `notElem` adjacentSquares || c == Empty
    where
        adjacentSquares = [Map.lookup (sumCoords coo offset) grid | offset <- [(-1,0),(0,1),(1,0),(0,-1)]]


{- Based on a list of the possible cells and the coordinate to fill in, and the current grid 
        - Get the current 'random' index (starting with integer based on the player's name)
        - Use this to pick a cell out of the avilable ones, and add it to the grid if it is valid 
            \-> Also update the random integer according to another random metric (size of the grid)
        - If it is not valid, try the next available cell
-}
fillInCell :: [Cell] -> Int -> Coord -> State (Int, Map.Map Coord Cell) Cell
fillInCell possibleCells rand coord = do
    (curInt, curGrid) <- get
    let
        possiblyNewCell = possibleCells !! (curInt `mod` length possibleCells)
        newCell = if allowCell possiblyNewCell coord curGrid then do
                        let newGrid = snd $ Map.updateLookupWithKey (\ _ _ -> Just possiblyNewCell) coord curGrid
                        put (curInt + rand, newGrid)
                        return possiblyNewCell
                    else do
                        put (curInt + 1, curGrid)
                        fillInCell possibleCells rand coord
    newCell

{- Take a list of available colours, a player name and an extra random integer (size of grid) and build a 2x2 pattern accordingly
        - If there are only 1 or 2 colours, use the interesting patterns to ensure a wide variety
        - If there are more than 2, use fillInCell to create a 'random' pattern
            \-> The random method creates (c+1)^2 patterns for c colours based on the given parameters
-}
create2x2 :: [Colour] -> Player -> Int -> Grid
create2x2 availableColours playerName rand
    | length availableColours == 1  = do
                                        let randIndex = (sum [ord l | l <-playerName]) `mod` 2
                                            randGrid = interesting1colourPatterns (head availableColours) !! randIndex
                                        Grid {width=2, height=2, rep=randGrid}
    | length availableColours == 2  = do
                                        let randIndex = (sum [ord l | l <-playerName]) `mod` 14
                                            randGrid = interesting2colourPatterns (head availableColours) (last availableColours) !! randIndex
                                        Grid {width=2, height=2, rep=randGrid}
    | otherwise                     = do
                                        let possibleCells = Empty : [Filled c | c  <- availableColours]
                                            empty2x2 = Map.fromList $ zip [(0,0),(0,1),(1,0),(1,1)] [Empty, Empty, Empty, Empty] -- empty 2x2 grid
                                            -- Go through each coordinate and fill in it's cell
                                            finalState = traverse (fillInCell possibleCells rand) [(0,0),(0,1),(1,0),(1,1)]
                                            -- Get the final state of the above, with the initial random integer as the player name's ASCII values
                                            (_, grid) = execState finalState (sum [ord l | l <-playerName], empty2x2)
                                        Grid {width=2, height=2, rep=grid}

        -- Unfortunatelly, the random method can give incomplete patterns 
        --     (ie. they are impossible to get since there are too many empty cells for the game to be over)
        -- With the randomised patterns, there are (c+1)^2 patterns for c colours, and 3/16 for 3 colours are incomplete
        -- This may encourage the player to be more on the offensive and ensure the other player does not get their pattern

-- There are only two interesting 2x2 patterns for 1-colour, so it's better to choose from these than randomise
interesting1colourPatterns :: Colour -> [Map.Map Coord Cell]
interesting1colourPatterns c1 = map (Map.fromList . zip [(0,0),(0,1),(1,0),(1,1)])
                                [[Filled c1, Empty, Empty, Filled c1],
                                 [Empty, Filled c1, Filled c1, Empty]]

-- For 2x2 patterns, the random method doesn't give many options, and some suffer from too many empty cells
-- Therefore, it's a lot nicer to give users 'interesting' colour patterns, of which there are 14 so it's more difficult to get the same ones
-- This is a list of grids whihc make interesting 2x2 2-colour patterns
interesting2colourPatterns :: Colour -> Colour -> [Map.Map Coord Cell]
interesting2colourPatterns c1 c2 = map (Map.fromList . zip [(0,0),(0,1),(1,0),(1,1)])
                                [[Filled c1, Empty, Empty, Filled c1],
                                 [Empty, Filled c1, Filled c1, Empty],
                                 [Filled c2, Filled c1, Filled c1, Empty],
                                 [Filled c1, Filled c2, Empty, Filled c1],
                                 [Filled c1, Empty, Filled c2, Filled c1],
                                 [Empty, Filled c1, Filled c1, Filled c2],
                                 [Filled c1, Filled c2, Filled c2, Filled c1],

                                 [Filled c2, Empty, Empty, Filled c2], -- same 7 patterns, with colours reversed
                                 [Empty, Filled c2, Filled c2, Empty],
                                 [Filled c1, Filled c2, Filled c2, Empty],
                                 [Filled c2, Filled c1, Empty, Filled c2],
                                 [Filled c2, Empty, Filled c1, Filled c2],
                                 [Empty, Filled c2, Filled c2, Filled c1],
                                 [Filled c2, Filled c1, Filled c1, Filled c2]
                                ]

-- Based on the final game grid and a pattern grid, check the maximum number of cells that match the given pattern
patternPoints :: Grid -> Grid -> Int
patternPoints gameGrid patternGrid = maximum [Map.size (strictIntersect (rep gameGrid) p) | p <- patterns] -- Checks the pattern across the whole gameGrid
    where
        -- Maps the pattern onto different offset coords of 2x2 grids inside a larger grid
        --      eg. in a 3x3 it starts with [(0,0), (0,1), (1,0), (1,1)]
        --          and ends with [(1,1), (1,2), (2,1), (2,2)]
        --        + ------------- +
        --        | (0,0)   (1,0) |  (2,0)              <- This coords are then mapped to the cells of the 2x2 grid
        --        |       + ------|------- +                    eg. [Filled Red, Empty, Empty, Filled Red]
        --        | (0,1) | (1,1) |  (2,1) |
        --        + ------|------ +        |
        --          (0,2) | (1,2)    (2,2) |
        --                + -------------- +

        offsetCoords = [ [ sumCoords startingCoords offset | offset <- [(0,0),(0,1),(1,0),(1,1)]] 
                                |  startingCoords <- [(h, w) | h <- [0..height gameGrid-2], w <- [0..width gameGrid-2]]]
        patterns = [ Map.fromList $ zip coords (map snd (Map.toList $ rep patternGrid)) | coords <- offsetCoords]

-- Map.intersect returns intersection based only on the keys, ignoring the values (which doesn't work since all grids will have the same coord keys)
-- strictIntersect returns intersection if the (key,value) pairs are the exact same
strictIntersect :: Map.Map Coord Cell -> Map.Map Coord Cell -> Map.Map Coord Cell
strictIntersect grid1 grid2 = Map.mapMaybe id $ Map.intersectionWith (\v1 v2 -> if v1 == v2 then Just v1 else Nothing) grid1 grid2