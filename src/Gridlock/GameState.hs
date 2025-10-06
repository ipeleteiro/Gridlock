module Gridlock.GameState ( initGameState, runGridlock, processMove, checkIfGameEnded, sumCoords ) where

import Gridlock.Types

import Data.List ((\\))
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Map as Map

import Control.Monad.State.Lazy

-- Helper function used to add two coordinates together element-wise
sumCoords :: (Num a, Num b) => (a, b) -> (a, b) -> (a, b)
sumCoords (a1, a2) (b1, b2) = (a1+b1, a2+b2)

-- Initialise a GameState based on the provided information
initGameState :: [Player] -> Set Colour -> (Int, Int) -> GameState
initGameState players colours (w, h) = GameState {nextPlayer=head players, curGrid= buildEmptyGrid, availColours=colours, availPlayers=players}
    where
        buildEmptyGrid = Grid {width=w, height=h, rep=Map.fromList [((r,c),Empty) | c <-[0..h-1], r<-[0..w-1]]}

-- Returns list of either grids or errors, and a bool of whether the game has actually ended yet
runGridlock :: [Move] -> GameState -> ([Either String Grid], Bool)
runGridlock moves initGame = (grids, isGameEnded)
    where
        finalState = traverse processMove moves
        grids = evalState finalState initGame
        isGameEnded = checkIfGameEnded (execState finalState initGame)

-- For a given move, checks if it is valid - updates the gird is yes, otherwise returns the error
processMove :: Move -> State GameState (Either String Grid)
processMove move = do
    gameState <- get
    case isValidMove move gameState of
        Right () -> do
            let newGrid = applyMove move (curGrid gameState)
            let newPlayer = head $ availPlayers gameState \\ [player move] -- sets the other player as next player
            put gameState {curGrid = newGrid, nextPlayer = newPlayer}
            return $ Right newGrid
        Left errorString -> return $ Left errorString

-- Applies a given move to the grid (other functions ensure this move is valid)
applyMove :: Move -> Grid -> Grid
applyMove move oldGrid = oldGrid {rep = snd newRep} -- only the new grid is needed so snd
    where
        -- updates the cell no matter the current value, returns (Maybe Cell, Map Coord Cell) - Maybe Cell will always be Just, but is ignored
        newRep = Map.updateLookupWithKey (\ _ _ -> Just (Filled (colour move))) (coord move) (rep oldGrid)

-- Ensures a move is valid based on the current GameState, returns Right () if valid, Left String for an error
isValidMove :: Move -> GameState -> Either String ()
isValidMove move gameState =
    let
        -- Checks that the cell is not out of bounds
        noBoundsError = unless (Map.member (coord move) (rep (curGrid gameState))) (fail "The cell is out of bounds")
        
        -- Gets adjacent squares to the cell a player is trying to colour, and checks if there are cells of the same colour surrounding it
        adjacentSquares = [Map.lookup (sumCoords (coord move) offset) (rep (curGrid gameState)) | offset <- [(-1,0),(0,1),(1,0),(0,-1)]]
        noAdjacentError = when (Just (Filled (colour move)) `elem` adjacentSquares) (fail "The colour must not be the same as an adjacent cell.")

        -- Checks that the correct player is playing
        noPlayerError = unless (nextPlayer gameState == player move) (fail "This player has just played, wrong player.")

        -- Checks that the colour is available in the game
        noColourError = unless (colour move `elem` availColours gameState) (fail "This colour is not available within the game.")

        -- Checks that the cell is Empty (not already filled in)
        noAlreadyFilledError = unless (Map.lookup (coord move) (rep (curGrid gameState)) == Just Empty) (fail "This cell is already filled in")
    in
        noBoundsError <<>> noAdjacentError <<>> noPlayerError <<>> noColourError <<>> noAlreadyFilledError
        -- <<>> is used so that if an error is encountered, the first on the chain is flagged

-- Adapts fail for (Either String) so it returns a Left String
instance MonadFail (Either String) where
  fail :: forall a. String -> Either String a
  fail = Left

-- New operator to chain possible errors (always prioritises errors)
(<<>>) :: Either a b -> Either a b -> Either a b
Left x <<>> _ = Left x
_ <<>> Left x = Left x
Right x <<>> Right _ = Right x

-- Iterates through empty cells to check if there is a valid move remaining - if there is not, the game has ended
checkIfGameEnded :: GameState -> Bool
checkIfGameEnded gameState = not $ any hasValidMove emptyCells
    where
        emptyCells = [coord | (coord, Empty) <- Map.toList (rep (curGrid gameState))]

        hasValidMove :: Coord -> Bool
        hasValidMove c =
            --                       constructs a move to test if it would be valid (using valid player and coords)
            any (\col -> isValidMove (Move {player = nextPlayer gameState, colour = col, coord = c}) gameState == Right ())
                (Set.toList (availColours gameState)) -- checks for each colour, if adding it to the grid would be a valid move