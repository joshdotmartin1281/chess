module Engine where

import qualified Data.Map as Map

data Color = White | Black
    deriving (Show)

data Rank = R1 | R2 | R3 | R4 | R5 | R6 | R7 | R8
    deriving (Eq, Ord, Show)
data File = A | B | C | D | E | F | G | H
    deriving (Eq, Ord, Show)

data PieceType = Pawn | Knight | Bishop | Rook | Queen | King
    deriving (Show)
data Piece = Piece Color PieceType
    deriving (Show)

data Square = Square
    { file :: File
    , rank :: Rank
    }
    deriving (Eq, Ord, Show)

newtype Board = Board (Map.Map Square Piece)
--    deriving (show)

--instance Show Board where
--    show board = 

emptyBoard :: Board
emptyBoard = Board Map.empty

placePiece :: Square -> Piece -> Board -> Board
placePiece sq piece (Board b) = Board (Map.insert sq piece b)

removePiece :: Square -> Board -> Board
removePiece sq (Board b) = Board (Map.delete sq b)

pieceAt :: Square -> Board -> Maybe Piece
pieceAt sq (Board b) = Map.lookup sq b

pieceChar :: Piece -> Char
pieceChar (Piece White Pawn)   = 'P'
pieceChar (Piece White Knight) = 'N'
pieceChar (Piece White Bishop) = 'B'
pieceChar (Piece White Rook)   = 'R'
pieceChar (Piece White Queen)  = 'Q'
pieceChar (Piece White King)   = 'K'

pieceChar (Piece Black Pawn)   = 'p'
pieceChar (Piece Black Knight) = 'n'
pieceChar (Piece Black Bishop) = 'b'
pieceChar (Piece Black Rook)   = 'r'
pieceChar (Piece Black Queen)  = 'q'
pieceChar (Piece Black King)   = 'k'

squareChar :: Board -> Square -> Char
squareChar board sq =
    case pieceAt sq board of
        Nothing -> '.'
        Just p  -> pieceChar p
