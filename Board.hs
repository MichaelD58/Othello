module Board where

import Data.Map as Map
import Data.List as List

data Colour = Black | White | Empty
  deriving (Show, Eq, Read)

other :: Colour -> Colour
other Black = White
other White = Black

type Position = (Int, Int)

-- A Board is a record containing the board size (a board is a square grid, n *
-- n), the number of consecutive passes, and a list of pairs of position and
-- the colour at that position.

data Board = Board { size :: Int,
                     passes :: Int,
                     pieces :: Map.Map Position Colour
                   }
  deriving (Show, Eq)

-- Default board is 8x8, neither played has passed, with 4 initial pieces 
initBoard :: Int -> Board
initBoard s = do let emptyGrid = Map.fromList (zip [(x, y) | x <- [0..s], y <- [0..s]] (repeat Empty))
                 let toAdd = getToAdd s
                 Board s 0 (Map.union toAdd emptyGrid)

getToAdd :: Integral b => b -> Map (b, b) Colour
getToAdd s = do let v1 = div s 2
                let v2 = v1 - 1
                Map.fromList (zip [(v2,v2), (v2,v1), (v1,v1), (v1,v2)] [White, Black, White, Black])

-- Overall state is the board and whose turn it is, plus any further
-- information about the world (this may later include, for example, player
-- names, which is the computer player, timers, information about 
-- rule variants, etc)
--
-- Feel free to extend this, and 'Board' above with anything you think
-- will be useful (information for the AI, for example, such as where the
-- most recent moves were).
data GameState 
       = GameState { board :: Board,
                     turn :: Colour,
                     prevstate :: Maybe GameState,
                     human :: Colour,
                     hints :: Bool,
                     turn_time :: Int,
                     difficulty :: Bool }
  deriving Eq

initGameState :: Colour -> Int -> Bool -> Int -> Bool -> GameState
initGameState c s h t d = GameState (initBoard s) Black Nothing c h t d

-- Play a move on the board; return 'Nothing' if the move is invalid
-- (e.g. outside the range of the board, there is a piece already there,
-- or the move does not flip any opposing pieces)
makeMove :: Board -> Colour -> Position -> Maybe Board
makeMove b c (x,y) = if (x >= 0) && (x < (Board.size b)) && (y >= 0) && (y < (Board.size b))
                     then do if (pieces b) !? (x,y) /= Nothing
                             then if (pieces b) ! (x,y) == Empty then (checkValid b c (x,y)) else Nothing
                             else Nothing
                     else Nothing

-- Attempt to make a move - could succeed or fail
attemptMove :: GameState -> Position -> GameState
attemptMove st p = do let newBoard = makeMove (board st) (turn st) p
                      if (show newBoard == "Nothing")
                      then st
                      else GameState (convertBoard newBoard) (other (turn st)) (Just st) (human st) (hints st) (turn_time st) (difficulty st)

makeCustomMove :: (Eq a, Num a) => a -> Board -> Colour -> (Int, Int) -> Maybe Board
makeCustomMove num b c (x,y) = if (pieces b) !? (x,y) /= Nothing
                          then if ((pieces b) ! (x,y) == Empty) && (num == 4) 
                               then Just (Board (Board.size b) 0 (Map.union (Map.fromList [((x,y),c)]) (pieces b)))
                               else if ((pieces b) ! (x,y) == Empty)
                                    then (checkValid' b c (x,y)) 
                                    else Nothing
                          else do Nothing

checkValid' :: Board -> Colour -> (Int, Int) -> Maybe Board
checkValid' b c (x,y) = if (getSurround b (x,y) /= 0)
                        then Just (Board (Board.size b) 0 (Map.union (Map.fromList [((x,y),c)]) (pieces b)))
                        else Nothing

getSurround :: Board -> (Int, Int) -> Int
getSurround b (x,y) = do let surround = [(0,1), (0,-1), (1,0), (-1,0), (1,1), (-1,1), (1,-1), (-1,-1)]
                         let colours = [(x+xo,y+yo) | (xo,yo) <- surround, (containsPiece b (x+xo,y+yo)) == True]
                         length colours

-- move is only valid if it is into an empty space that will flip at least one tile
checkValid :: Board -> Colour -> (Int, Int) -> Maybe Board
checkValid b c (x,y) = if (containsPiece b (x, y) == False) && (length (piecesToFlip b (x, y) c) > 0)
                     then Just (performFlips (Board (Board.size b) 0 (Map.union (Map.fromList [((x,y),c)]) (pieces b))) (piecesToFlip b (x, y) c))
                     else Nothing

convertGameState :: Maybe GameState -> GameState
convertGameState x = case x of
                     Just x -> x

convertBoard :: Maybe Board -> Board
convertBoard x = case x of
                  Just x -> x

-- collects all the pieces that will be flipped after a move for the given colour to the given tile
piecesToFlip :: Board -> Position -> Colour -> [Position]
piecesToFlip b p c = List.concat (List.map (checkSurroundingPiece b c [] p) [(0,1), (0,-1), (1,0), (-1,0), (1,1), (-1,1), (1,-1), (-1,-1)])

-- returns false if the coord is out of bounds or that space is empty
containsPiece :: Board -> Position -> Bool
containsPiece b (x, y) = if (x >= Board.size b || x < 0) || (y >= Board.size b || y < 0)
                            then False
                            else if ((pieces b) ! (x, y)) /= Empty
                                 then True
                                 else False

-- checks a row of pieces in a given direction
-- takes the board, the position of the tile just placed, the offset to indicate direction, the current list of pieces being flipped (initially empty) and the colour to flip to
checkSurroundingPiece :: Board -> Colour -> [Position] -> Position -> Position -> [Position]
checkSurroundingPiece b c rlist (x, y) (xo, yo) | containsPiece b (x+xo,y+yo) && ((pieces b) ! (x+xo, y+yo)) /= c = checkSurroundingPiece b c ((x+xo,y+yo):rlist) (x+xo,y+yo) (xo, yo)
                                                | containsPiece b (x+xo,y+yo) = rlist
                                                | otherwise = []

-- takes the list of coordinates to flip the colour of and performs those flips, returning a new board
performFlips :: Board -> [Position] -> Board
performFlips b (x:[]) = individualFlip b x
performFlips b (x:xs) = performFlips (individualFlip b x) xs

-- flips the colour of a single coordinate and returns a new board
individualFlip :: Board -> Position -> Board
individualFlip b pos = Board (Board.size b) (passes b) (adjust (other) pos (pieces b))

-- Check the current score
-- Returns a pair of the number of black pieces, and the number of
-- white pieces
checkScore :: Board -> (Int, Int)
checkScore b = do let xs = Map.elems (pieces b)
                  let blacks = [x | x <- xs, x == Black]
                  let whites = [x | x <- xs, x == White]
                  ((length blacks), (length whites))

-- Return true if the game is complete (that is, either the board is
-- full or there have been two consecutive passes)
gameOver :: Board -> Bool
gameOver b
       | (passes b) == 2 = True
       | (fst (checkScore b)) + (snd (checkScore b)) == ((Board.size b)*(Board.size b)) = True
       | otherwise = False

-- An evaluation function for a minimax search. Given a board and a colour
-- return an integer indicating how good the board is for that colour.
evaluate :: Board -> Colour -> Int
evaluate b c = do let xs = Map.keys (pieces b)
                  let moves = [x | x <- xs, (show (makeMove b c x)) /= "Nothing"]
                  length moves

printWinner :: Ord b => (b, b) -> [Char]
printWinner score
          | (fst score) > (snd score) = "Black wins!"
          | (fst score) < (snd score) = "White wins!"
          | otherwise = "Draw!"
