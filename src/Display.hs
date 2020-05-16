module Display where

import Data.Map as Map
import Data.List as List

import Board

{- Displaying the board. For example, you could render it as follows,
 - where 'O' is a white piece and '*' is a black piece:
 
   A B C D E F G H
 1 . . . . . . . .
 2 . . . . . . . .
 3 . . . . . . . .
 4 . . . O * . . .
 5 . . . * O . . .
 6 . . . . . . . .
 7 . . . . . . . .
 8 . . . . . . . .
 
 -}

-- Given a game state, return a String which represents the state of the
-- board.
--
-- This will need to extract the Board from the world state and draw it
-- as a grid plus pieces.

printSymbol :: Colour -> String
printSymbol x | x == White = "0"
              | x == Black = "X"
              | otherwise  = "."

printRow :: GameState -> Int -> String
printRow g row = " " ++ show row ++ " " ++ (intercalate " " (List.map (\position -> printSymbol (pieces (board g) ! position)) ([(x, row) | x <- [0..(Board.size (board g))-1]]))) ++ " \n"

showGameState :: GameState -> String
showGameState g = 
                   "\n" ++
                   "  " ++ List.take (Board.size (board g)*2) " A B C D E F G H I J" ++ "\n" ++
                   (intercalate "" (List.map (printRow g) $ [0..(Board.size (board g))-1]))