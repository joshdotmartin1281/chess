module Engine.Moves where

import Engine.Types
import Engine.Board


startRank :: Color -> Rank
startRank White = R2
startRank Black = R7


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

bishopOffsets =
    [ (1,1)
    , (1,-1)
    , (-1,1)
    , (-1,-1)
    ]

rookOffsets = 
    [ (1,0)
    , (0,1)
    , (-1,0)
    , (0,-1)
    ]


kingOffsets = 
    bishopOffsets ++ rookOffsets


queenOffsets =
    bishopOffsets ++ rookOffsets


captureOffsets color =
    [(-1, pawnDirection color), (1, pawnDirection color)]

pawnDirection :: Color -> Int
pawnDirection White = 1
pawnDirection Black = -1


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


bishopMoves :: Position -> Color -> Square -> [Move]
bishopMoves pos color fromSq =
    concatMap (ray pos color fromSq) bishopOffsets


rookMoves :: Position -> Color -> Square -> [Move]
rookMoves pos color fromSq = 
    concatMap (ray pos color fromSq) rookOffsets


queenMoves :: Position -> Color -> Square -> [Move]
queenMoves pos color fromSq =
    concatMap (ray pos color fromSq) queenOffsets


kingMoves :: Position -> Color -> Square -> [Move]
kingMoves pos color fromSq = 
    [ Move fromSq toSq Nothing
    | offset <- kingOffsets
    , Just toSq <- [offsetSquare fromSq offset]
    , canMoveTo pos color toSq
    ]


pawnMoves :: Position -> Color -> Square -> [Move]
pawnMoves pos color from =
    singleMove ++ doubleMove ++ captureMoves
  where
    singleMove =
        case offsetSquare from (0, pawnDirection color) of
            Just to
                | pieceAt to (board pos) == Nothing ->
                    [Move from to Nothing]
            _ ->
                []
    doubleMove =
        case from of
            Square _ r
                | r == startRank color ->
                    case ( offsetSquare from (0, pawnDirection color)
                         , offsetSquare from (0, 2 * pawnDirection color) ) of
                        (Just one, Just two)
                            | pieceAt one (board pos) == Nothing
                            , pieceAt two (board pos) == Nothing ->
                                [Move from two Nothing]
                        _ ->
                            []
            _ ->
                []
    captureMoves =
        [ Move from to Nothing
        | offset <- captureOffsets color
        , Just to <- [offsetSquare from offset]
        , Just (Piece c _) <- [pieceAt to (board pos)]
        , c /= color
        ]


ray :: Position -> Color -> Square -> (Int, Int) -> [Move]
ray pos color fromSq dir = go fromSq
  where
    go sq =
      case offsetSquare sq dir of
        Nothing -> []

        Just next ->
          case pieceAt next (board pos) of
            Nothing ->
              Move fromSq next Nothing : go next

            Just (Piece c _)
              | c /= color -> [Move fromSq next Nothing]
              | otherwise  -> []


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

