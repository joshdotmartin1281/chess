module Main where

import Engine.Types
import Engine.Board
import Engine.Moves
import Engine.Render

main :: IO ()
main = putStrLn (renderBoard initialBoard)

