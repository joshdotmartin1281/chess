module Engine.Render where

import Engine.Types
import Engine.Board
import Engine.MoveGen

renderPos :: Position -> String
renderPos (Position b c cr ep hm fm) =
    "Side to move: " ++ show c
    ++ "\nCastling rights: " ++ show cr
    ++ "\nEn passant target: " ++ show ep
    ++ "\nHalfmove clock: " ++ show hm
    ++ "\nFull move clock: " ++ show fm
    ++ "\n" ++ renderBoard b


renderMoves :: Position -> Square -> String
renderMoves pos from =
    renderWith renderSquare
  where
    b = board pos
    legalTargets = [to | Move _ to _ <- pieceMoves pos from]

    renderSquare sq
        | sq == from             = '@'
        | sq `elem` legalTargets = '&'
        | otherwise              = squareChar b sq


renderWith :: (Square -> Char) -> String
renderWith squareFn =
    unlines $
        border :
        concatMap renderRank ranks ++
        [fileLabels]
  where
    files = [A, B, C, D, E, F, G, H]
    ranks = [R8, R7, R6, R5, R4, R3, R2, R1]

    border = "  + - + - + - + - + - + - + - + - +"

    renderRank r =
        [ showRank r ++ " |"
            ++ concatMap (\f -> " " ++ [squareFn (Square f r)] ++ " |") files
        , border
        ]

    fileLabels = "    A   B   C   D   E   F   G   H"

    showRank r = case r of
        R1 -> "1"
        R2 -> "2"
        R3 -> "3"
        R4 -> "4"
        R5 -> "5"
        R6 -> "6"
        R7 -> "7"
        R8 -> "8"


renderBoard :: Board -> String
renderBoard b =
    renderWith (squareChar b)


