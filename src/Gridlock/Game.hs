{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use bimap" #-}
module Gridlock.Game where

import qualified Data.Set as Set
import Data.List ((\\))

import Gridlock.GameState
import Gridlock.Types
import Gridlock.Parser
import Gridlock.GridPatterns
import Data.Char (isDigit)

import Control.Monad.State.Lazy
import Gridlock.DrawGrid (drawGrid)
import Data.Either (rights, lefts)

-- "Parses" the user input get valid information and return a valid GameSetUp
parseGameSetUp :: IO GameSetUp
parseGameSetUp = do
    putStrLn "Game grid size (nxm):"
    gridSize <- getGridSize

    putStrLn "Players (comma-separated):"
    players <- getPlayers

    putStrLn "Colours (comma-separated):"
    colours <- getColours

    -- For each player, create a 2x2 pattern as their goal, with random parameters of their names and the size of the grid
    let chosenPatterns = (create2x2 colours (head players) (fst gridSize), create2x2 colours (last players) (snd gridSize))
    let gameInit = GameSetUp {gameGridSize=gridSize, patternGrids=chosenPatterns, playerNames=players, gameColours=Set.fromList colours}
    return gameInit

-- Expects user input of nxm, and returns this as (n,m) - if the input is not valid, the user must re-input it
-- For the patterns to work, the grids must be at least 2x2, so it also checks for that
getGridSize :: IO (Int, Int)
getGridSize = do
    input <- getLine
    let size :: Maybe (Int, Int)
        size = case splitBy 'x' input of
                [w, h] | all isDigit w && all isDigit h -> Just (read w, read h)
                _ -> Nothing

    case size of
        Nothing -> do
                putStrLn "Grid size must be of the form nxm:"  -- The input was invalid, so input again
                getGridSize
        Just (w, h) ->  if w<2 || h<2 then do
                            putStrLn "The size of the grid must be at least 2x2:"  -- The sizes of the grid were too small, so input again
                            getGridSize 
                        else                  
                            return (w, h)

-- Expects a comma-seperated list of colours, where all colours are valid and there is at least one colour, otherwise asks the use to re-input
getColours :: IO [Colour]
getColours = do
    input <- getLine
    let maybeColours = map matchColour (splitBy ',' (filter (/= ' ') input))
    if not (null maybeColours) && null (lefts maybeColours) then    -- if all colours are valid, return a list of colours
        return $ rights maybeColours
    else do
        putStrLn "Please enter valid colours, comma-separated (and at least one colour):"
        getColours

-- Expects a comma-separated list of players, where there is exactly two players, otherwise asks the use to re-input
getPlayers :: IO [Player]
getPlayers = do
    input <- getLine
    case length (splitBy ',' (filter (/= ' ') input)) of
        2 -> return $ splitBy ',' (filter (/= ' ') input)
        _ -> do
                putStrLn "Please enter TWO players, comma-separated:"
                getPlayers

{- Allows users to play Gridlock through the terminal as a kind of command-line
        - The set up of the game is 'parsed' through with parseGameSetUp
        - The game displays the patterns for the two players to aim towards at the start
        - Players then take turns inputting coordinates and colours for each of their moves
            \-> If the user makes an invalid move, the program lets them re-input the move (with an appropriate error)
            \-> Errors like "wrong player" and "player not in the game" cannot happen anymore
        - Once the game is over, the player whihc moved last is awarded 2 points
        - The game then calculates how many cells the players filled in correctly for their patterns, adding 1 point per cell
        - The overall winner is displayed
 -}
playGridlock :: IO ()
playGridlock = do
    setUp <- parseGameSetUp
    let p1 = head (playerNames setUp)
        p2 = last (playerNames setUp)

    putStrLn $ "The pattern for " <> p1 <> " is:"       -- Displaying goal patterns
    putStrLn $ drawGrid $ fst $ patternGrids setUp
    putStrLn $ "The pattern for " <> p2 <> " is:"
    putStrLn $ drawGrid $ snd $ patternGrids setUp

    let initGame = initGameState (playerNames setUp) (gameColours setUp) (gameGridSize setUp)
        curState = execState (startGame initGame) initGame  -- initialises the State

    putStrLn $ "The starting player is: " <> nextPlayer initGame    -- Player 1 always starts the game
    (loser, finalGrid) <- gamePlay curState                         -- Run through the player moves, returning the loser and final grid
    let winner = head (playerNames setUp \\ [loser])

    putStrLn "The final grid is:"
    putStrLn $ drawGrid finalGrid

    putStrLn $ "The current winner is " <> winner <> ", gaining 2 points, but let's check the pattern scores!"
    putStrLn $ "Score for " <> p1 <> " is " <> show (patternPoints finalGrid (fst (patternGrids setUp)))  -- Calculates extra pattern points for players
    putStrLn $ "Score for " <> p2 <> " is " <> show (patternPoints finalGrid (snd (patternGrids setUp)))
    putStrLn "So the overall scores are..."
    let p1FinalScore = if p1 == winner then patternPoints finalGrid (fst (patternGrids setUp)) + 2  -- Adds 2 points to the original winner
                        else patternPoints finalGrid (fst (patternGrids setUp))
        p2FinalScore = if p2 == winner then patternPoints finalGrid (snd (patternGrids setUp)) + 2
                        else patternPoints finalGrid (snd (patternGrids setUp))
        overallWinner = if p1FinalScore >= p2FinalScore then p1 else p2

    putStrLn $ show p1FinalScore <> " points for " <> p1 <> "."
    putStrLn $ show p2FinalScore <> " points for " <> p2 <> "."
    putStrLn $ "Making " <> overallWinner <> " the overall winner!!"

-- With the current GameState, allows players to input a move and:
--      - displays error if needed, allowing player to re-input
--      - returns to playGridlock if the game is over
--      - otherwise returns itself to allow the next player to move
gamePlay :: GameState -> IO (Player, Grid)
gamePlay curState = do
        curMove <- getMove (nextPlayer curState)    -- get the player move
        let newGameState = processMove curMove
        case evalState newGameState curState of     -- check if the move is valid
            Left e -> do
                putStrLn e  -- display error if invalid move
                gamePlay curState -- keep the current state and allow player to re-try
            Right updatedGrid -> if checkIfGameEnded (execState newGameState curState) then do         
                                     -- When the game has ended, return the loser and final grid
                                    return (nextPlayer (execState newGameState curState), updatedGrid)
                                 else do
                                    putStrLn $ drawGrid updatedGrid     -- display the new grid
                                    putStrLn $ "The next player is: " <> nextPlayer (execState newGameState curState)
                                    gamePlay (execState newGameState curState) -- update the game state and go to the next move

-- Initialises the State based on the starting GameState
startGame :: GameState -> State GameState (Either String Grid)
startGame initGame = do
    put initGame
    return $ Right $ curGrid initGame

-- "Parses" user input, asking for the coordinates and colour the player wishes to use, returning the info as a Move record
getMove :: Player -> IO Move
getMove playerName = do
    putStrLn "Play coord n,m: "
    c <- getCoord

    putStrLn "Colour to play: "
    col <- getColour

    return $ Move {player=playerName, coord=c, colour=col}

-- Expects a coordinate as n,m - asking for the user to re-input if this is not provided
getCoord :: IO (Int, Int)
getCoord = do
    input <- getLine
    let size :: Maybe (Int, Int)
        size = case splitBy ',' input of
                [w, h] | all isDigit w && all isDigit h -> Just (read w, read h)
                _ -> Nothing

    case size of
        Nothing -> do
                    putStrLn "Coord must be of the form n,m:"
                    getCoord
        Just (w, h) -> return (w, h)

-- Expects a singular colour, whihc if invalid the user must re-input
getColour :: IO Colour
getColour = do
    input <- getLine
    case matchColour input of
        Left _ -> do
                    putStrLn "Please give valid colour:"
                    getColour
        Right c -> return c



