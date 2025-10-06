module Gridlock.TUITypes where

import Gridlock.Types
import Brick

-- | A TUIState is the current state of the complete GRIDLOCK game being displayed.
data TUIState = TUIState 
    {
        selectedCell :: Maybe (Cell, Coord),
        -- ^ Nothing if a cell isn't selected, the cell and it's coordinate otherwise.
        selectedColour :: Maybe Colour,
        -- ^ Nothing if a colour isn't selected, the selected colour otherwise.
        gameState :: GameState,
        -- ^ The current game state.
        gameSetUp :: GameSetUp,
        -- ^ The information of the game set up, passed to the TUI app.
        logLines :: [Widget Interactable]
        -- ^ List of all log lines, kept as widgets (all strWrap).
    }

-- | Interactable sets the types of posisble interactions within the game
data Interactable
    = CellBtn (Int, Int) -- The coordinates of the selected Cell
    | PlayBtn Player -- The player which pressed the button
    | ColourBtn Colour -- The selected colour
    | LogScroll
    deriving (Eq, Ord, Show)