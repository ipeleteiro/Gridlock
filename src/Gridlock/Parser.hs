{-# OPTIONS_GHC -Wno-unused-do-bind #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Gridlock.Parser ( parseGame, splitBy, matchColour ) where

import Text.Megaparsec hiding (State) -- makes sure to use Control.Monad.State
import Text.Megaparsec.Char

import qualified Data.Set as Set

import Data.List ((\\))

import Data.Void (Void)
import Gridlock.Types
import Gridlock.GameState ( initGameState, runGridlock )
import Data.Char (toLower)

import Data.Either (lefts, rights)


type Parser = Parsec Void String

{- | Parse a game into a GameRecord. -}
parseGame :: Parser GameRecord
parseGame = do
    string' "GRIDLOCK\nPlayers: "
    players <- parsePlayers

    string' "Grid: "
    (w, h) <- parseGrid

    string' ".\nColours: "
    colours <- parseColours

    skipMany (string "\n")
    moves <- many (parsePlayerCommand players) -- gets list of all moves (or (player, player) for the end moves)
    let
        validMoves = lefts moves -- list of all player moves in order
        initialGameState = initGameState players colours (w, h) -- initialises a game with the given information

        winningPlayer = if null validMoves then last players else player $ last validMoves -- the actual winner based on moves
        losingPlayer = if null validMoves then head players else head $ players \\ [winningPlayer] -- the actual loser based on moves

        givenWinner = snd $ head (rights moves) -- the given winner in the file
        givenLoser = fst $ head (rights moves) -- the loser winner in the file

        -- Runs Gridlock through list of moves, getting the overall girds and wether the game has actually ended
        (gameGrids, isGameEnded) = runGridlock validMoves initialGameState  
        -- Gets a list of all errors, including wether the game has actually ended
        errors = if isGameEnded then lefts gameGrids else lefts gameGrids <> ["The game has not ended"]

    if errors /= [] then
        fail (head errors) -- Shows the first error found
    else
        -- Checks that the given winner and loser is correct
        if losingPlayer == givenLoser then
            if winningPlayer == givenWinner then
                -- And if all is correct, returns the final GameRecord
                pure $ GameRecord {players=players, colours=colours, grids=curGrid initialGameState : rights gameGrids}
            else fail "Wrong winner."
        else fail "Wrong loser."

-- Addapted words - https://hackage.haskell.org/package/ghc-internal-9.1201.0/docs/src/GHC.Internal.Data.OldList.html#words
splitBy :: Char -> [Char] -> [[Char]]
splitBy c s =  case dropWhile (==c) s of
                        "" -> []
                        s' -> w : splitBy c s''
                                where (w, s'') =
                                        break (==c) s'

-- Expects a grid size definition in the form nxm, where n and m are both only digits (ie. positive whole numbers)
parseGrid :: Parser (Int, Int)
parseGrid = do
    w <- many digitChar
    char 'x'
    h <- many digitChar
    return (read w, read h)     -- safely returns the width and height as (Int, Int)

-- Expects a list of players separated by commas and ending with ".", ensures that exactly two players are given
parsePlayers :: Parser [Player]
parsePlayers = do
    line <- manyTill anySingle (string ".\n")
    let players = splitBy ',' (filter (/= ' ') line)
    if length players == 2 then
        pure players
    else
        fail "There should be exactly two players in the game."

-- Expects a list of colours separated by commas and ending with ".", ensures that all colours are valid and at least one is given
parseColours :: Parser (Set.Set Colour)
parseColours = do
    line <- manyTill anySingle (string ".\n")
    let colours = splitBy ',' (filter (/= ' ') line)
    if not (null colours) then
        pure $ Set.fromList $ rights $ map matchColour colours      -- returns set of colours (if all colours are valid)
    else
        fail "There should be at least one colour given."

-- Starts parsing all lines that start with a player name, then continues to parse a move or the end of the game
parsePlayerCommand :: [Player] -> Parser (Either Move (Player, Player))
parsePlayerCommand availPlayers = do
    playerName <- many $ satisfy (' '<)

    -- Checks if the player is part of the available players, then checks if it can parse a move or the end of the game
    if playerName `elem` availPlayers || playerName=="" then (Left <$> parseMove playerName) <|> (Right <$> parseEndMove playerName)
    else fail "This player doesn't exist."

-- Parses the end of the game ([player] cannot move. [player] wins!), returns the winning and losing players
parseEndMove :: Player -> Parser (Player, Player)
parseEndMove playerName = do
    string " cannot move.\n"
    winner <- many $ satisfy (' '<)
    string " wins!"
    pure (playerName, winner)

-- Parses a move ([player] plays [colour] at [coord].) and returns the information wrapped in a Move record
parseMove :: Player -> Parser Move
parseMove playerName = do
    string " plays "
    colourString <- many $ satisfy (' '<)
    string " at "
    coordInput <- parseCoord
    string ".\n"
    case matchColour colourString of
        Right c ->  pure Move {player=playerName, colour=c, coord=coordInput}
        Left e -> fail e

-- Expects a coordinate in the form (n, m), and returns the coord wrapped as a Coord
parseCoord :: Parser Coord
parseCoord = do
    char '('
    x <- many digitChar
    char ','
    y <- many digitChar
    char ')'
    pure (read x, read y)

-- Turns a string representation of a colour into a Colour (or fails if the string given does not match a colour)
matchColour :: String -> Either String Colour
matchColour x = case map toLower x of
                "red" -> Right Red
                "green" -> Right Green
                "yellow" -> Right Yellow
                "blue" -> Right Blue
                "magenta" -> Right Magenta
                "cyan" -> Right Cyan
                _ -> fail "We don't have that colour :("
