module Gridlock.DrawGrid where

import Gridlock.ColourSquares (square)
import Gridlock.Types

-- Hint: These might come in handy :)
import Data.Map qualified as Map


-- Draws grid by surrounding the cells around 'border's, iterating through the map cells to turn them into strings
drawGrid :: Grid -> String
drawGrid g = border ++ "\n" ++ concat [row i | i <- [0.. height g-1]] ++ border
    where
        border = "+" ++ replicate (width g) '-' ++ "+"
        row n = "|" ++ concat [drawCell (i, n) g | i <- [0..width g -1]] ++ "|" ++ "\n"

-- Based on some coords and a grid, finds the cell and returns the appropraite string representation
drawCell :: Coord -> Grid -> String
drawCell (col, row) g = case Map.findWithDefault Empty (col, row) (rep g) of       
                                Empty -> " "
                                Filled colour -> square colour
                        -- if a cell is not found by coord (should not be possible, but just in case), it is set to a default Empty 
                        

    
