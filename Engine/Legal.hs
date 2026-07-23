module Engine.Legal
    ( makeLegalMove
    ) where 

import Engine.Board
import Engine.Types
import Engine.MakeMove
import Engine.MoveGen

makeLegalMove :: Move -> Position -> Maybe Position 
makeLegalMove move pos 
    | isLegalMove move pos = Just (makeMove move pos)
    | otherwise            = Nothing


isLegalMove :: Move -> Position -> Bool
isLegalMove move pos = False


inCheck :: Color -> Position -> Bool
inCheck move pos = False


squareAttacked :: Square -> Position -> Bool
squareAttacked sqr pos = False


