module Engine.Types where

import qualified Data.Map as Map

data Color = White | Black
    deriving (Eq, Show)

data Rank = R1 | R2 | R3 | R4 | R5 | R6 | R7 | R8
    deriving (Eq, Ord, Enum, Bounded, Show)

data File = A | B | C | D | E | F | G | H
    deriving (Eq, Ord, Enum, Bounded, Show)

data PieceType = Pawn | Knight | Bishop | Rook | Queen | King
    deriving (Eq, Show)

data Piece = Piece Color PieceType
    deriving (Eq, Show)

data Square = Square
    { file :: File
    , rank :: Rank
    }
    deriving (Eq, Ord, Show)

data Move = Move
    { from :: Square
    , to :: Square
    , promotion    :: Maybe PieceType
    }
    deriving (Show)

data CastlingRights = CastlingRights
    { whiteKingSide  :: Bool
    , whiteQueenSide :: Bool
    , blackKingSide  :: Bool
    , blackQueenSide :: Bool
    }
    deriving (Eq, Show)

data Position = Position
    { board            :: Board
    , sideToMove       :: Color
    , castlingRights   :: CastlingRights
    , enPassantTarget  :: Maybe Square
    , halfmoveClock    :: Int
    , fullmoveNumber   :: Int
    }
    deriving (Show)

newtype Board = Board (Map.Map Square Piece)
    deriving (Show)

