module AI where

import Board
import Input
import Data.Map as Map
import Data.List as List
import Data.Ord
import System.Random

data GameTree = GameTree { game_board :: Board,
                           game_turn :: Colour,
                           next_moves :: [(Position, Int, GameTree)] } -- Map with position as a key and a tuple value containing the number of pieces that move would flip and the subsequent predicted gameTree

-- Given a function to generate plausible moves (i.e. board positions)
-- for a player (Col) on a particular board, generate a (potentially)
-- infinite game tree.
--
-- (It's not actually infinite since the board is finite, but it's sufficiently
-- big that you might as well consider it infinite!)
--
-- An important part of the AI is the 'gen' function you pass in here.
-- Rather than generating every possible move (which would result in an
-- unmanageably large game tree!) it could, for example, generate moves
-- according to various simpler strategies.

-- Chooses a custom start position
startingMoves :: Board -> Colour -> Position
startingMoves b c = let positions = Map.keys (pieces b)
                        moves = [x | x <- positions, (getSurround b x) /= 0]
                    in if length moves == 0 
                       then head (List.take ((\(a,b)->a) (randomR (0, (length positions)-1) (mkStdGen 0))) positions) -- generates a random move if it is placing custom start position before human
                       else maximum moves

-- Returns a list of up to five moves that flip the most pieces
getHints :: Board -> Colour -> [Position]
                  -- Produces a list of tuples containing a coord on the left and the number of pieces that would be flipped by moving there on the right
getHints b c = do let flipCountList = (List.zip [(x, y) | x <- [0..((Board.size b) -1)], y <- [0..((Board.size b) -1)]] (List.map (\(i,j) -> length (piecesToFlip b (i,j) c)) [(x, y) | x <- [0..((Board.size b) -1)], y <- [0..((Board.size b) -1)]]))
                  -- Sorts the list in descending order of flip count, filters out invlaid moves or moves that flip 0 pieces, and maps to return only the coord list in this order, before selecting the top five moves
                  List.map (\((x, y), z) -> (x, y)) (List.take 5 (List.filter (\((x, y), z) -> if z > 0 && (containsPiece b (x, y) == False) then True else False) (reverse (sortBy (comparing snd) (flipCountList)))))

buildTree :: (Board -> Colour -> [Position]) -- ^ Move generator (e.g. hints function)
             -> Board -- ^ board state
             -> Colour -- ^ player to play next
             -> GameTree
buildTree gen b c = let moves = gen b c in -- generated moves
                        GameTree b c (mkNextStates moves)
  where
    mkNextStates :: [Position] -> [(Position, Int, GameTree)]
    mkNextStates [] = []
    mkNextStates (pos : xs)
        = case makeMove b c pos of -- try making the suggested move
               Nothing -> mkNextStates xs -- not successful, no new state
               Just b' -> (pos, (length (piecesToFlip b pos c)), (buildTree gen b' (other c))) : mkNextStates xs
                             -- successful, make move and build tree from here for opposite player

-- Get the best next move from a (possibly infinite) game tree. This should
-- traverse the game tree up to a certain depth, and pick the move which
-- leads to the position with the best score for the player whose turn it
-- is at the top of the game tree.
getHardMove :: Int -- ^ Maximum search depth
               -> GameTree -- ^ Initial game tree
               -> Position
getHardMove depth tree = let sortedTree = (reverse (sortBy (comparing (\(x,y,z)->y)) (next_moves tree))) in
                         if depth == 0
                         then (\(x,y,z)->x) (head sortedTree)
                         else do let moves = [(((\(a,b,c)->a) x), (getHardMove (depth - 1) ((\(a,b,c)->c) x))) | x <- (reverse (sortBy (comparing (\(x,y,z)->y)) (next_moves tree))), length (next_moves ((\(a,b,c)->c) x)) > 0]
                                 if (length moves) == 0 then getHardMove (depth - 1) tree
                                 else do let (x:xs) = moves
                                         let newMove = getMax' (game_board tree) (game_turn tree) x xs
                                         fst newMove

getMax b c n [] = n
getMax b c n [x] = if (getScore b c (fst n)) > (getScore b c (fst x)) then n else x
getMax b c n (x:xs) = if (getScore b c (fst n)) > (getScore b c (fst x)) then getMax b c n xs else getMax b c x xs

getMax' b c n [] = n
getMax' b c n [x] = if (getScore b c (snd n)) > (getScore b c (snd x)) then n else x
getMax' b c n (x:xs) = if (getScore b c (snd n)) > (getScore b c (snd x)) then getMax b c n xs else getMax b c x xs

-- Gets the worst move by doing the opposite of getHardMove, used for easy difficulty
getEasyMove :: Int -- ^ Maximum search depth
               -> GameTree -- ^ Initial game tree
               -> Position
getEasyMove depth tree = let sortedTree = (sortBy (comparing (\(x,y,z)->y)) (next_moves tree)) in
                         if depth == 0
                         then (\(x,y,z)->x) (head sortedTree)
                         else do let moves = [(((\(a,b,c)->a) x), (getEasyMove (depth - 1) ((\(a,b,c)->c) x))) | x <- (sortBy (comparing (\(x,y,z)->y)) (next_moves tree)), length (next_moves ((\(a,b,c)->c) x)) > 0]
                                 if (length moves) == 0 then getEasyMove (depth - 1) tree
                                 else do let (x:xs) = moves
                                         let newMove = getMin' (game_board tree) (game_turn tree) x xs
                                         fst newMove

getMin  b c n [] = n
getMin b c n [x] = if (getScore b c (fst n)) > (getScore b c (fst x)) then x else n
getMin b c n (x:xs) = if (getScore b c (fst n)) > (getScore b c (fst x)) then getMin b c x xs else getMin b c n xs

getMin' b c n [] = n
getMin' b c n [x] = if (getScore b c (snd n)) > (getScore b c (snd x)) then x else n
getMin' b c n (x:xs) = if (getScore b c (snd n)) > (getScore b c (snd x)) then getMin b c x xs else getMin b c n xs

getScore b c pos = do let move = makeMove b c pos
                      if (show move) == "Nothing"
                      then 0
                      else evaluate (convertBoard move) c

-- Have the AI make a move and return the new game state
updateGameState :: GameState -- ^ current game state
                   -> GameState -- ^ new game state after computer move
updateGameState st = do let tree = buildTree getHints (board st) (turn st)
                        if difficulty st
                        then let move = getHardMove 1 tree
                                 newBoard = makeMove (board st) (turn st) move
                             in GameState (convertBoard newBoard) (other (turn st)) (Just st) (human st) (hints st) (turn_time st) (difficulty st)
                        else let move = getEasyMove 1 tree
                                 newBoard = makeMove (board st) (turn st) move
                             in GameState (convertBoard newBoard) (other (turn st)) (Just st) (human st) (hints st) (turn_time st) (difficulty st)

