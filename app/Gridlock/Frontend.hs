{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use isNothing" #-}
module Gridlock.Frontend where

import Brick

import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.List ((\\))

import Gridlock.GameState ( checkIfGameEnded, initGameState, processMove )
import Gridlock.Types
import Gridlock.GridPatterns ( patternPoints )

import Control.Monad.State.Lazy

import Gridlock.TUITypes
import Graphics.Vty
import Brick.Widgets.Center ( hCenter )
import Brick.Widgets.Border ( border, hBorder, vBorder )
import System.Exit (exitSuccess)

{- ACKNOWLEDGEMENT
    - This code takes inspiration from the halatro frontend and the Brick github guide (https://github.com/jtdaugherty/brick/blob/master/docs/guide.rst)
    - No code was copied, only looked at to better understand how Brick works
-}

-- Sets up the TUI app and initial state, and starts running it
runTUI :: GameSetUp -> IO ()
runTUI setUp = do
    let app = App {
            appDraw = drawScreen,
            appChooseCursor = neverShowCursor,
            appHandleEvent = handleEvent,
            appAttrMap = const allAttributes,
            appStartEvent = do
                vty <- getVtyHandle
                let output = outputIface vty
                when (supportsMode output Mouse) $     
                    liftIO $ setMode output Mouse True  -- allows mouse clicking 
        }
        initialState = TUIState {
            selectedCell = Nothing,     -- Ensures no cell or colour is currently selected
            selectedColour = Nothing,
            gameState = initGameState (playerNames setUp) (gameColours setUp) (gameGridSize setUp), -- initialises the GameState
            gameSetUp = setUp,  -- the setUp is passed from command-line user input before the tui app starts
            -- Initliases the log to have the instructions to play GRIDLOCK, including how scoring works
            logLines = [strWrap "Welcome to Gridlock!", strWrap "ere are the rules in case you haven't played before :)",
                        strWrap "   - On your turn you can select an empty cell and a colour and 'play' it, as long as it is not adjacent to another cell of the same colour.",
                        strWrap "   - If a player can no longer move, the game is over, and the 'winning' player gets 2 points.",
                        strWrap "   - But they may not win! Both players have patterns/goals to reach, which if found on the board, will give you points for however many cells you have correctly filled in.",
                        strWrap "       ╚═> eg: a partially completed goal of 3 cells gives 3 points.",
                        strWrap "       ╚═> if the grids are the same it's just nromal gridlock!",
                        strWrap "   - The overall winner is the player with most points.",
                        strWrap "Good luck!"    ]
        }
    _ <- defaultMain app initialState
    return ()

{- Creates the visual screen of the game based on it's current state -}
drawScreen :: TUIState -> [Widget Interactable]
drawScreen tuistate =
    -- a <=> b = a on top of b
    [
        vBox [
            hBox [
                border (
                    -- Title and current grid, which allows each cell to be selected (if empty)
                    hCenter (withAttr (attrName "title") (str title)) 
                        <=> padBottom (Pad 2) (hCenter (renderGrid (selectedCell tuistate) True (curGrid gs)))
                ),
                hLimit 70 $ vBox [
                    border (
                        -- The available colours, all always clickable
                        padRight (Pad 2) $ str "Colours:" 
                            <=> hBox [ renderColour (selectedColour tuistate) c | c <- Set.toList (availColours gs)]
                    ),
                    hBorder,
                    -- The log, which can be scrolled through
                    str "Log:",
                    vLimit 10 $ viewport LogScroll Vertical $ vBox (logLines tuistate),
                    str "\nPress q to quit" -- always on screen
                ]
            ],
            border $ hBox [
                -- Player 1's section, including their button to play and their pattern goal
                padLeft (Pad 2) $ padBottom (Pad 8) (str p1) 
                    <=> playBtn (nextPlayer gs == p1) p1,
                padLeft (Pad 2) $ padRight (Pad 2) (str "Goal" 
                    <=> renderGrid (selectedCell tuistate) False (fst $ patternGrids setUp)),  -- False since the patterns are not clickable
        
                padLeft (Pad 2) $ padRight (Pad 2) vBorder, -- border and padding to separate them

                -- Player 2's section, including their button to play and their pattern goal
                padBottom (Pad 8) (str p2) 
                    <=> playBtn (nextPlayer gs == p2) p2,
                padLeft (Pad 2) $ padRight (Pad 2) (str "Goal" 
                    <=> renderGrid (selectedCell tuistate) False (snd $ patternGrids setUp))
            ]
        ]
    ]
    where
        gs = gameState tuistate
        setUp = gameSetUp tuistate
        p1 = head $ playerNames setUp
        p2 = last $ playerNames setUp

{- All possible events within the game -}
handleEvent :: BrickEvent Interactable () -> EventM Interactable TUIState ()
-- If an empty cell is selected, update the selectedCell appropriately 
handleEvent (MouseDown (CellBtn (r, c))  _ _ _) = do
    tuiState <- get
    let cell = Map.findWithDefault Empty (r,c) (rep (curGrid (gameState tuiState)))
    put tuiState {selectedCell=Just (cell, (r,c))}

-- If an colour is selected, update the selectedColour appropriately 
handleEvent (MouseDown (ColourBtn c)  _ _ _) = do
    tuiState <- get
    put tuiState {selectedColour=Just c}

-- If a player button is selected, check that a colour and cell is selected and process the move if so
handleEvent (MouseDown (PlayBtn p)  _ _ _) = do
    tuistate <- get
    -- If a cell or colour is not selected, print appropriate error on the log
    if selectedCell tuistate == Nothing || selectedColour tuistate == Nothing then
        put tuistate {logLines=logLines tuistate <> [strWrap "Please select a cell and a colour."]}
    else do
        let Just (_, selectedCoords) = selectedCell tuistate    -- technically 'unmatched', but the if above ensures both are Just values
            Just c = selectedColour tuistate
            -- Build a move based on the current player, cell and colour selected
            curMove = Move {player=p, coord=selectedCoords, colour=c}
            -- And an appropriate log (mirrors .gridlock files)
            curLog = strWrap (p <> " plays " <> reverseMatchColour c <> " at " <> showCoords selectedCoords)

            curState = gameState tuistate
            newGameState = processMove curMove
            
        case evalState newGameState curState of
            Left e -> do    -- Log error if encountered, allowing the player to reselect a cell and colour
                put tuistate {logLines=logLines tuistate <> [strWrap e]}
                
            Right updatedGrid -> do
                -- Final game sequence, figuring out scores and winners in the same way as in Game.hs
                if checkIfGameEnded (execState newGameState curState) then do
                    let winner = p -- last player to press the play button, renamed to make code easier to follow
                        setUp = gameSetUp tuistate
                        p1 = head $ playerNames setUp
                        p2 = last $ playerNames setUp

                        p1PatternPoints = patternPoints updatedGrid (fst (patternGrids setUp)) -- Calculates extra pattern points for players
                        p2PatternPoints = patternPoints updatedGrid (snd (patternGrids setUp))

                        p1FinalScore = if p1 == winner then p1PatternPoints + 2 else p1PatternPoints -- Adds 2 points to the original winner
                        p2FinalScore = if p2 == winner then p2PatternPoints + 2 else p2PatternPoints
                        overallWinner = if p1FinalScore >= p2FinalScore then p1 else p2

                    put tuistate {gameState=curState {curGrid=updatedGrid},
                                    selectedCell=Nothing,
                                    selectedColour=Nothing,
                                    -- Final log updates, displaying scores and final winner
                                    logLines=logLines tuistate <> [curLog]
                                            <> [strWrap ("The current winner is " <> winner <> ", gaining 2 points, but let's check the pattern scores!")]
                                            <> [strWrap ("Score for " <> p1 <> " is " <> show p1PatternPoints)]
                                            <> [strWrap ("Score for " <> p2 <> " is " <> show p2PatternPoints)]
                                            <> [strWrap "So the overall scores are..."]
                                            <> [strWrap (show p1FinalScore <> " points for " <> p1 <> ".")]
                                            <> [strWrap (show p2FinalScore <> " points for " <> p2 <> ".")]
                                            <> [strWrap ("Making " <> overallWinner <> " the overall winner!!")]
                                }
                else
                    -- Update the grid on screen, de-select the cell and colour, and update the log
                    put tuistate {gameState=curState {curGrid=updatedGrid, nextPlayer=head $ availPlayers curState \\ [p]},
                                    selectedCell=Nothing,
                                    selectedColour=Nothing,
                                    logLines=logLines tuistate <> [curLog]}

-- If the key 'q' is pressed, exit the program
handleEvent (Brick.VtyEvent (EvKey (KChar 'q') [])) = liftIO exitSuccess
-- If the cursor is over the log and the mouse scrolls, then scroll the log in correct direction
handleEvent (MouseDown LogScroll BScrollDown _ _) = vScrollBy (viewportScroll LogScroll) 1
handleEvent (MouseDown LogScroll BScrollUp _ _) = vScrollBy (viewportScroll LogScroll) (-1)
-- For any other event, ensure the log is scrolled to the end (then do nothing)
handleEvent _ = vScrollToEnd (viewportScroll LogScroll)

{- All attributes GRIDLOCK uses -}
allAttributes :: AttrMap
allAttributes = attrMap defAttr
    [
      (attrName "title", fg brightMagenta),
      (attrName "canPlay", white `on` blue),            -- player can currently play
      (attrName "cannotPlay", white `on` brightBlack),  -- player cannot currently play
      (attrName "red", bg red),
      (attrName "yellow", bg yellow),
      (attrName "green", bg green),
      (attrName "blue", bg blue),
      (attrName "cyan", bg cyan),
      (attrName "magenta", bg magenta),
      (attrName "emptyCell", bg brightBlack)
    ]

-- Turns a Colour into a string, used for the log and for matching colours to attributes
reverseMatchColour :: Colour -> String
reverseMatchColour x = case x of
                Red -> "red"
                Green -> "green"
                Yellow -> "yellow"
                Blue -> "blue"
                Magenta -> "magenta"
                Cyan -> "cyan"

-- Turns coordinates into string so it's easily printed out
showCoords :: (Int, Int) -> String
showCoords (x, y) = "(" <> show x <> "," <> show y <> ")"

-- Based on the current grid, current selected cell, and wether cells are clickable, displays the grid visually
renderGrid :: Maybe (Cell, Coord) -> Bool -> Grid -> Widget Interactable
renderGrid curSelected isClickable grid = vBox [
                        -- Ireate thorugh each row and column in the grid and render cells appropriatelly
                        hBox [
                            renderCell curSelected isClickable (Map.findWithDefault Empty (row, column) (rep grid)) (row, column)
                            | column <- [0..width grid -1]
                            ]
                        | row <- [0..height grid -1]
                        ]

-- Based on the given cell, current selected cell, and wether cells are clickable, displays the cell visually
renderCell :: Maybe (Cell, Coord) -> Bool -> Cell -> (Int, Int) -> Widget Interactable
renderCell curSelected isClickable cell (row, col) = case cell of
                    -- None of the coloured cells are clickable, so display them normally
                    Filled c -> padTop (Pad 1) $ padLeft (Pad 2) $ vLimit 5 $ hLimit 7 $
                                        withAttr (attrName $ reverseMatchColour c) $ padAll 1 $ hCenter (txt " ")
                    _  -> if isClickable then
                                if Just (cell, (row,col)) == curSelected then
                                    -- If the cell is clickable, and currently cliked, display it with a border around it
                                    clickable (CellBtn (row, col)) selected
                                else
                                    -- If it is clickable, but not clicked, allow clicking but diplay without border
                                    clickable (CellBtn (row, col)) $ padTop (Pad 1) $ padLeft (Pad 2) emptyCell
                          -- If it is not clickable, simply display an empty cell
                          else padTop (Pad 1) $ padLeft (Pad 2) emptyCell
        where emptyCell = vLimit 5 $ hLimit 7 $
                                withAttr (attrName "emptyCell") $ padAll 1 $ hCenter (txt " ")
              selected =  border emptyCell

-- Based on the given colour and current selected colour, display the colour
renderColour :: Maybe Colour -> Colour -> Widget Interactable
renderColour curSelected c =
                if Just c == curSelected then
                    -- Add a border if the colour is currently selected
                    clickable (ColourBtn c) (border colourBtn)
                else
                    clickable (ColourBtn c) (padTop (Pad 1) $ padLeft (Pad 2) colourBtn)
        where colourBtn = vLimit 10 $ hLimit 10 $
                    withAttr (attrName $ reverseMatchColour c) $ padAll 1 $ hCenter (txt " ")

-- Display a blue "Play" button if the given player can play, a grey "Wait" non-clickable button otherwise
playBtn :: Bool -> Player -> Widget Interactable
playBtn canPlay p
    | canPlay    = clickable (PlayBtn p) (padRight (Pad 10) $ padLeft (Pad 10) $ hLimit 30 $ withAttr (attrName "canPlay") $ padAll 1 $ hCenter (txt "Play"))
    | otherwise  = padRight (Pad 10) $ padLeft (Pad 10) $ hLimit 30 (withAttr (attrName "cannotPlay") $ padAll 1 $ hCenter (txt "Wait"))

-- GRIDLOCK title! 
title :: String
title = concat [  " █████╗ ██████╗ ██████╗██████╗ ██╗     ██████╗  ██████╗ ██╗  ██╗\n"
                , "██╔═══╝ ██╔══██╗╚═██╔═╝██╔══██╗██║    ██╔═══██╗██╔═══██╗██║██╔═╝\n"
                , "██║ ███╗██████╔╝  ██║  ██║  ██║██║    ██║   ██║██║   ╚═╝████╔╝  \n"
                , "██║  ██║██╔══██║  ██║  ██║  ██║██║    ██║   ██║██║   ██╗██║██╗  \n"
                , "╚█████╔╝██║  ██║██████╗██████╔╝██████╗╚██████╔╝╚██████╔╝██║ ║██╗\n"
                , " ╚════╝ ╚═╝  ╚═╝╚═════╝╚═════╝ ╚═════╝ ╚═════╝  ╚═════╝ ╚═╝  ╚═╝ " ]
-- (/very/ inspired by the HALATRO title)
