module Engine.MakeMove
    ( makeMove
    ) where

import Engine.Types
import Engine.Board

makeMove move pos =
    case pieceAt (from move) (board pos) of
        Nothing -> pos
        Just piece ->
            let
                newBoard =
                    placePiece (to move) piece $
                    removePiece (from move) (board pos)

                newSide =
                    case sideToMove pos of
                        White -> Black
                        Black -> White

                newFullmove =
                    case sideToMove pos of
                        Black -> fullmoveNumber pos + 1
                        White -> fullmoveNumber pos
            in
            pos
                { board = newBoard
                , sideToMove = newSide
                , fullmoveNumber = newFullmove
                }


