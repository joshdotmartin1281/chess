module Engine.Legal
    ( makeLegalMove
    ) where 

import Engine.Board
import Engine.Types
import Engine.Moves

makeLegalMove :: Move -> Position -> Maybe Position 
makeLegalMove move pos 
    | isLegalMove move pos = Just (makeMove move pos)
    | otherwise            = Nothing


isLegalMove :: Move -> Position -> Bool
isLegalMove move pos = move `elem` allMoves pos &&
    not (inCheck (sideToMove pos) (makeMove move pos))


inCheck :: Color -> Position -> Bool
inCheck move pos = False


squareAttacked :: Square -> Position -> Bool
squareAttacked sqr pos = False


