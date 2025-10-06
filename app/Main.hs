-- This is the main entry point for your Gridlock application.

import Gridlock.DrawGrid ( drawGrid )
import Gridlock.Parser ( parseGame )
import Gridlock.Types
import Gridlock.Game ( parseGameSetUp, playGridlock )
import Gridlock.Frontend ( runTUI )
import System.Environment ( getArgs )
import Text.Megaparsec ( runParser, errorBundlePretty )



main :: IO ()
main = do
    args <- getArgs
    case args of
        [] -> do
                setUp <- parseGameSetUp  -- Get all the information needed to ste up the game from terminal user input
                runTUI setUp  -- Run the game
        
        -- Slightly restrictive to expect 'command-line' arguement, but it's much more likely the TUI version is called
        ["command-line"] -> do playGridlock

        (filePath:_) -> do
            -- I stored all the given Gridlock games in /allGridLockGames for easier access, so only the file name has to be given
            let fullPath = "CHANGE THIS" <> filePath
            fileContents <- readFile fullPath -- Gives error if file is not found
            case runParser parseGame fullPath fileContents of
                Left err -> putStrLn ("Parsing failed:\n" <> errorBundlePretty err) -- Prints error if found while parsing
                Right gameRecord ->  mapM_ (putStrLn . drawGrid) (grids gameRecord) -- Otherwise prints all of the grids in order

-- Files with apostrophes must be passed with "" -> stack run -- "08-wrong-person-can't-move.gridlock"