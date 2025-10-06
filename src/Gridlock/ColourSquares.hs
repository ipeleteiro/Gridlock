module Gridlock.ColourSquares where

import Gridlock.Types (Colour (..))

import System.Console.ANSI (
  ColorIntensity (..),
  ConsoleLayer (..),
  SGR (..),
  setSGRCode,
 )
import System.Console.ANSI qualified as ANSI

-- This module defines a function for working with the terminal, which will allow you to print squares of a particular colour to the command line.

-- | Get a coloured square as a string.
square :: Colour -> String
square c =
  concat
    [ setSGRCode [SetColor Background Dull (toANSI c)]
    , [char c]
    , setSGRCode [Reset]
    ]
 where
  toANSI = \case
    Red -> ANSI.Red
    Green -> ANSI.Green
    Yellow -> ANSI.Yellow
    Blue -> ANSI.Blue
    Magenta -> ANSI.Magenta
    Cyan -> ANSI.Cyan
  char = \case
    Red -> 'R'
    Green -> 'G'
    Yellow -> 'Y'
    Blue -> 'B'
    Magenta -> 'M'
    Cyan -> 'C'
