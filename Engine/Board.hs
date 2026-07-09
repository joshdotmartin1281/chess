module Engine.Board where 

import Engine.Types
import qualified Data.Map as Map

renderBoard :: Board -> String
renderBoard board = 
        unlines $
            "+ - + - + - + - + - + - + - + - +" :
            concatMap renderRank ranks
        where
            files = [A, B, C, D, E, F, G, H]
            ranks = [R8, R7, R6, R5, R4, R3, R2, R1]
            renderRank r = 
                [ "|" ++ concatMap (\f -> " " ++ [squareChar board (Square f r)] ++ " |") files 
                    , "+ - + - + - + - + - + - + - + - +"
                ]

emptyBoard :: Board
emptyBoard = Board Map.empty

placePiece :: Square -> Piece -> Board -> Board
placePiece sq piece (Board b) = Board (Map.insert sq piece b)

removePiece :: Square -> Board -> Board
removePiece sq (Board b) = Board (Map.delete sq b)

movePiece :: Square -> Square -> Board -> Board
movePiece start end board =
    case pieceAt start board of
        Nothing -> board
        Just piece -> placePiece end piece (removePiece start board)

pieceAt :: Square -> Board -> Maybe Piece
pieceAt sq (Board b) = Map.lookup sq b

occupiedSquares :: Board -> [Square]
occupiedSquares (Board b) = Map.keys b

allSquares :: [Square]
allSquares =
    [ Square f r
    | r <- [R1, R2, R3, R4, R5, R6, R7, R8]
    , f <- [A, B, C, D, E, F, G, H]
    ]


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
        Nothing -> '*'
        Just p  -> pieceChar p


initialBoard :: Board
initialBoard = foldr (\(sq, p) b -> placePiece sq p b) emptyBoard pieces
  where
    pieces =
        [ (Square A R1, Piece White Rook)
        , (Square B R1, Piece White Knight)
        , (Square C R1, Piece White Bishop)
        , (Square D R1, Piece White Queen)
        , (Square E R1, Piece White King)
        , (Square F R1, Piece White Bishop)
        , (Square G R1, Piece White Knight)
        , (Square H R1, Piece White Rook)

        , (Square A R2, Piece White Pawn)
        , (Square B R2, Piece White Pawn)
        , (Square C R2, Piece White Pawn)
        , (Square D R2, Piece White Pawn)
        , (Square E R2, Piece White Pawn)
        , (Square F R2, Piece White Pawn)
        , (Square G R2, Piece White Pawn)
        , (Square H R2, Piece White Pawn)

        , (Square A R7, Piece Black Pawn)
        , (Square B R7, Piece Black Pawn)
        , (Square C R7, Piece Black Pawn)
        , (Square D R7, Piece Black Pawn)
        , (Square E R7, Piece Black Pawn)
        , (Square F R7, Piece Black Pawn)
        , (Square G R7, Piece Black Pawn)
        , (Square H R7, Piece Black Pawn)

        , (Square A R8, Piece Black Rook)
        , (Square B R8, Piece Black Knight)
        , (Square C R8, Piece Black Bishop)
        , (Square E R8, Piece Black King)
        , (Square D R8, Piece Black Queen) 
        , (Square F R8, Piece Black Bishop)
        , (Square G R8, Piece Black Knight)
        , (Square H R8, Piece Black Rook)
        ]

