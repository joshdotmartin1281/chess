module Engine.Moves where

import Engine.Types
import Engine.Board

makeMove :: Move -> Position -> Position
makeMove move pos = case pieceAt (from move) (board pos) of
        Nothing -> pos
        Just piece ->let newBoard = placePiece (to move) piece (removePiece (from move) (board pos))
            in pos { board = newBoard
                   , sideToMove = 
                        case sideToMove pos of
                        White -> Black
                        Black -> White
                    }

initialPosition :: Position
initialPosition = Position
        { board = initialBoard
        , sideToMove = White
        , castlingRights =
            CastlingRights
                { whiteKingSide = True
                , whiteQueenSide = True
                , blackKingSide = True
                , blackQueenSide = True
                }
        , enPassantTarget = Nothing
        , halfmoveClock = 0
        , fullmoveNumber = 1
        }

