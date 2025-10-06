module Gridlock.Types where

import Data.Map (Map)
import Data.Set (Set)

--------------------------------------------------------------------------------

-- | A player is identified by their name.
type Player = String

{- | A coordinate is a pair of integers.
  The first one is the x-coordinate (column number),
  and the second one is the y-coordinate (row number).
-}
type Coord = (Int, Int)

{- | A grid is a 2D array of cells.
  The grid is represented as a Map from Coords (a pair of Ints) to Cells.
-}
data Grid = Grid
    { width :: Int
    -- ^ The number of columns in the grid
    , height :: Int
    -- ^ The number of rows in the grid
    , rep :: Map Coord Cell
    -- ^ The internal representation of the grid
    }
    deriving (Eq, Ord, Read, Show)

-- | A cell is either empty or contains a colour.
data Cell = Empty | Filled Colour
    deriving (Eq, Ord, Read, Show)

-- | The set of colours is fixed in advance and corresponds to the six ANSI base terminal colours (white and black excluded).
data Colour = Red | Green | Yellow | Blue | Magenta | Cyan
    deriving (Eq, Ord, Bounded, Enum, Read, Show)

-- | A GameRecord is a complete record of a full game.
data GameRecord = GameRecord
    { players :: [Player]
    -- ^ The list of players in the game.
    , grids :: [Grid]
    -- ^ The list of grids as the game evolved.
    , colours :: Set Colour
    -- ^ The list of colours in the game.
    }
    deriving (Eq, Ord, Read, Show)

-- | A Move is a player action in a game of gridlock.
data Move = Move
    { player :: Player
    -- ^ The player making the action.
    , colour :: Colour
    -- ^ The colour of this action, one of the six ANSI base terminal colours (white and black excluded).
    , coord :: Coord
    -- ^ The coordinate of this move.
    }
    deriving (Eq, Ord, Read, Show)

-- | A GameState is the current state of the game of gridlock after 0+ moves.
data GameState = GameState
    {
        nextPlayer :: Player
        -- ^ The next player allowed to make a move.
    ,   availColours :: Set Colour
        -- ^ The set of colours in the game.
    ,   availPlayers :: [Player]
        -- ^ The list of players that can play.
    ,   curGrid :: Grid
        -- ^ The current grid at this point in the game (will always be valid grids).
    }
    deriving (Eq, Ord, Read, Show)

-- | A GameSetUp is the information given by users that wish to play the extension Gridlock game.
--          (Mainly used to pass information into GameState, and keep track of player's pattern grids)
data GameSetUp = GameSetUp
    {
        gameGridSize :: (Int, Int)
        -- ^ The size of the grids users have to chosen to play.
    ,   patternGrids :: (Grid, Grid)
        -- ^ The two pattern grids for each player.
    ,   playerNames :: [Player]
        -- ^ The list of players that can play.
    ,   gameColours :: Set Colour
        -- ^ The set of colours in the game.
    }
    deriving (Eq, Ord, Read, Show)
