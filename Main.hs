module Main where

import Engine.Types
import Engine.Board
import Engine.Moves

main :: IO ()
main = putStrLn (renderBoard initialBoard)

