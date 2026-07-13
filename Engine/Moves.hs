module Engine.Moves where

import Engine.Types
import Engine.Board

knightOffsets =
    [ (1,2)
    , (2,1)
    , (2,-1)
    , (1,-2)
    , (-1,-2)
    , (-2,-1)
    , (-2,1)
    , (-1,2)
    ]

offsetFile :: File -> Int -> Maybe File
offsetFile f d =
    let n = fromEnum f + d
    in if n >= fromEnum (minBound :: File)
          && n <= fromEnum (maxBound :: File)
       then Just (toEnum n)
       else Nothing

offsetRank :: Rank -> Int -> Maybe Rank
offsetRank r d =
    let n = fromEnum r + d
    in if n >= fromEnum (minBound :: Rank)
          && n <= fromEnum (maxBound :: Rank)
       then Just (toEnum n)
       else Nothing

offsetSquare :: Square -> (Int, Int) -> Maybe Square
offsetSquare (Square f r) (df, dr) = do
    f' <- offsetFile f df
    r' <- offsetRank r dr
    pure (Square f' r')


pieceMoves :: Position -> Square -> [Move]
pieceMoves pos sq =
    case pieceAt sq (board pos) of
        Nothing -> []
        Just (Piece color Pawn)   -> pawnMoves pos color sq
        Just (Piece color Knight) -> knightMoves pos color sq
        Just (Piece color Bishop) -> bishopMoves pos color sq
        Just (Piece color Rook)   -> rookMoves pos color sq
        Just (Piece color Queen)  -> queenMoves pos color sq
        Just (Piece color King)   -> kingMoves pos color sq


knightMoves :: Position -> Color -> Square -> [Move]
knightMoves pos color fromSq = 
    [ Move fromSq toSq Nothing 
    | offset <- knightOffsets
    , Just toSq <- [offsetSquare fromSq offset]
    , canMoveTo pos color toSq
    ]


pawnMoves _ _ _ = []
bishopMoves _ _ _ = []
rookMoves _ _ _ = []
queenMoves _ _ _ = []
kingMoves _ _ _ = []

canMoveTo :: Position -> Color -> Square -> Bool
canMoveTo pos color sq = 
    case pieceAt sq (board pos) of 
        Nothing -> True
        Just (Piece c _) -> c /= color


allMoves :: Position -> [Move]
allMoves pos =
    concatMap (pieceMoves pos) ownPieces
  where
    ownPieces =
        filter isMine (occupiedSquares (board pos))

    isMine sq =
        case pieceAt sq (board pos) of
            Just (Piece c _) -> c == sideToMove pos
            Nothing -> False


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

