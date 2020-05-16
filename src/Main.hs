module Main where

import Board
import Display
import Input
import AI
import Data.Map as Map
import Data.List as List
import System.Environment
import System.IO
import Control.Monad
import System.Directory
import Debug.Trace

-- Utility function that splits a string on a delimiter, used to split respecified args the same way as 'getArgs'
separateBy :: Eq a => a -> [a] -> [[a]]
separateBy chr = unfoldr sep where
  sep [] = Nothing
  sep l  = Just . fmap (List.drop 1) . break (== chr) $ l

-- Reverts the game state such that the human player's last move and the AI's last move are undone
revertGameState :: Colour -> GameState -> Int -> GameState
revertGameState hc (GameState b c Nothing _ h t d) _ = GameState b c Nothing hc h t d
revertGameState hc (GameState b c (Just prevstate) _ _ _ _) 1 = revertGameState hc prevstate 2
revertGameState hc (GameState b c (Just prevstate) _ _ _ _) 2 = prevstate

-- Saves a textual representation of the game to the given file name
saveGameToFile :: GameState -> [Char] -> IO()
saveGameToFile st f = do appendFile f ((show (passes (board st))) ++ "\n")
                         let lst = Map.toList (pieces (board st))
                         appendFile f (show lst)
                         return()

-- Evaluates possible inputs from the human player's move (separated from gameLoop to improve legibility)
moveOptions :: GameState -> [Char] -> IO()
moveOptions st move | move == "undo"    = do putStrLn("Attempting to revert to previous game state... ")
                                             gameLoop (revertGameState (human st) st 1)
                    | move == "restart" = do putStrLn("Restarting game... Would You Like to change game options? (y/n) ")
                                             response <- getLine
                                             if response == "n" then main
                                             else if response == "y" then do putStrLn("Please respecify command line arguments: ")
                                                                             args <- getLine
                                                                             processArgs (separateBy ' ' args) True st
                                             else moveOptions st move
                    | move == "pause"   = do putStrLn("Game stopped. Would You Like to change game options? (y/n) ")
                                             -- pauseGameCheck st
                                             response <- getLine
                                             if response == "n" then gameLoop st
                                             else if response == "y" then do putStrLn("NOTE - only player colour and hint settings can be changed without restarting the game")
                                                                             prevArgs <- getArgs
                                                                             putStrLn("Your previous settings were: " ++ show (intercalate " " (prevArgs)))
                                                                             putStrLn("Please respecify command line arguments: ")
                                                                             args <- getLine
                                                                             if length prevArgs == 0 
                                                                             then do if (length (separateBy ' ' args) /= 5) || (runningOptionsValidation ["Black", "8", "normal-start", "hints-off", "hard"] (separateBy ' ' args) == False)
                                                                                     then gameLoop st
                                                                                     else processArgs (separateBy ' ' args) False st
                                                                             else do if (length (separateBy ' ' args) /= 5) || (runningOptionsValidation prevArgs (separateBy ' ' args) == False)
                                                                                     then gameLoop st
                                                                                     else processArgs (separateBy ' ' args) False st
                                             else gameLoop st
                    | move == "quit"    = do putStrLn "save? (y/n)?"
                                             response <- getLine
                                             if response == "y"
                                             then saveGameToFile st "autosave.txt"
                                             else do writeFile "autosave.txt" ""
                                                     return()
                    | otherwise = let st' = (attemptMove st (getCoord move)) in
                                  if st' == st
                                  then do putStrLn "Error, please replay"
                                          gameLoop st
                                  else gameLoop st'
                            
-- 
startMoveOptions :: (Eq a, Num a) => GameState -> a -> [Char] -> IO ()
startMoveOptions st depth move | move == "restart" = do putStrLn("Restarting game... Would You Like to change game options? (y/n) ")
                                                        response <- getLine
                                                        if response == "n" then main
                                                        else if response == "y" then do putStrLn("Please respecify command line arguments: ")
                                                                                        args <- getLine
                                                                                        processArgs (separateBy ' ' args) True st
                                                                                        else customStartLoop depth st
                               | move == "quit"    = do putStrLn "save? (y/n)?"
                                                        response <- getLine
                                                        if response == "y"
                                                        then saveGameToFile st "autosave.txt"
                                                        else do writeFile "autosave.txt" ""
                                                        return()
                               | move == "pause"   = do putStrLn("Game stopped. Would You Like to change game options? (y/n) ")
                                                        -- pauseGameCheck st
                                                        response <- getLine
                                                        if response == "n" then customStartLoop depth st
                                                        else if response == "y" then do putStrLn("NOTE - only player colour and hint settings can be changed without restarting the game")
                                                                                        prevArgs <- getArgs
                                                                                        putStrLn("Your previous settings were: " ++ show (intercalate " " (prevArgs)))
                                                                                        putStrLn("Please respecify command line arguments: ")
                                                                                        args <- getLine
                                                                                        if length prevArgs == 0 
                                                                                        then do if (length (separateBy ' ' args) /= 5) || (runningOptionsValidation ["Black", "8", "normal-start", "hints-off", "hard"]  (separateBy ' ' args) == False)
                                                                                                then customStartLoop depth st
                                                                                                else processArgs (separateBy ' ' args) False st
                                                                                        else do if (length (separateBy ' ' args) /= 5) || (runningOptionsValidation prevArgs (separateBy ' ' args) == False)
                                                                                                then customStartLoop depth st
                                                                                                else processArgs (separateBy ' ' args) False st
                                                                                else customStartLoop depth st
                               | move == "undo" = do putStrLn("Attempting to revert to previous game state... ")
                                                     if (depth == 4) then customStartLoop depth st
                                                     else customStartLoop (depth+2) (revertGameState (human st) st 1)
                               | otherwise = do let (x,y) = getCoord move
                                                let newBoard = (placeCustomStart depth st (x,y))
                                                if (show newBoard) == "Nothing"
                                                then do putStrLn "Error, please replay"
                                                        customStartLoop depth st
                                                else do let st' = GameState (convertBoard newBoard) (other (turn st)) (Just st) (human st) (hints st) (turn_time st) (difficulty st)
                                                        customStartLoop (depth-1) st'
                        
-- Validation on new args when attempting to change game options without restarting the game
-- Board size and custom start settings cannot be changed
runningOptionsValidation :: [String] -> [String] -> Bool
runningOptionsValidation (a:b:c:d:e) (a':b':c':d':e') | b /= b' = False
                                                      | c /= c' = False
                                                      | otherwise = True 

-- Main game loop
gameLoop :: GameState -> IO ()
gameLoop st
        | (gameOver (board st)) = do putStrLn "Game Over"
                                     putStrLn (printWinner (checkScore (board st)))
        | ((evaluate (board st) (turn st)) > 0) = do putStrLn (showGameState st)
                                                     let (x,y) = checkScore (board st)
                                                     putStrLn ("Scores: Black = " ++ (show x) ++ " | White = " ++ (show y))
                                                     putStrLn ((show (turn st)) ++ " to move")  

                                                     -- Human Case
                                                     if (turn st) == (human st) 
                                                     then do when ((hints st) == True) $ putStrLn ("Suggested moves: " ++ show (List.map (\(i, j) -> (intToLetter i) ++ show(j)) (getHints (board st) (turn st))))
                                                             putStr "Move: "
                                                             hFlush stdout
                                                             hWaitForInput stdin (turn_time st) >>= \inputIsAvailable ->
                                                               if inputIsAvailable
                                                               then do move <- getLine
                                                                       moveOptions st move -- Evaluate human input
                                                               else do putStrLn "Out of time!"
                                                                       passGo st
                                                                                                               
                                                     -- AI Case
                                                     else gameLoop (updateGameState st)
        | otherwise = passGo st

-- Loop that runs before main game loop if custom starting positions are being used
-- Allows player and AI to choose their starting positions
customStartLoop :: (Eq a, Num a) => a -> GameState -> IO ()
customStartLoop depth st
                | depth == 0 = gameLoop st
                | otherwise = do putStrLn (showGameState st)
                                 putStrLn ((show (turn st)) ++ " to move")
                                 if (turn st) == (human st)
                                 -- Human case
                                 then do putStr "Move: "
                                         hFlush stdout
                                         move <- getLine
                                         startMoveOptions st (depth) move
                                 -- AI case
                                 else do let move = startingMoves (board st) (turn st)
                                         putStrLn ("Move: " ++ (intToLetter (fst (move))) ++ (show (snd move)))
                                         let st' = GameState (convertBoard (placeCustomStart depth st move)) (other (turn st)) (Just st) (human st) (hints st) (turn_time st) (difficulty st)
                                         customStartLoop (depth-1) st'

-- Method to place a custom starting position piece on the board, without flipping any other pieces, returning the new board
placeCustomStart :: (Eq a, Num a) => a -> GameState -> Position -> Maybe Board
placeCustomStart depth st p = makeCustomMove depth (board st) (turn st) p
                                 
-- Pass a turn
passGo :: GameState -> IO ()
passGo st = do putStrLn ((show (turn st)) ++ " passed.")
               let newBoard = Board (Board.size (board st)) ((passes (board st)) + 1) (pieces (board st))
               let st' = GameState newBoard (other (turn st)) (Just st) (human st) (hints st) (turn_time st) (difficulty st)
               gameLoop st'
                                                                        
-- Main method called when application is run
-- Deals with command line arguments before starting the game
main :: IO ()
main = do args <- getArgs
          processArgs args True (initGameState Black 8 False 60 True) -- Note that this is a dummy gamestate since it must be included as an arg for processArgs

-- Processes given set of arguments and calls relevant methods 
-- Boolean given as True unless seeking to change game settings without restarting, in which case provided game state is updated with new args
processArgs :: [String] -> Bool -> GameState -> IO ()
                           -- Do not restart the game - change settings in place
processArgs args flag st | flag == False = do let colourArg = (head args)
                                              let sizeArg = getInt ((\(x:y:xs) -> y) args)
                                              let customStartArg = ((\(x:y:z:xs) -> z) args)
                                              let hintArg = ((\(a:b:c:d:xs) -> d) args)
                                              let diffArg = (last args)
                                              writeFile "autosave.txt" (colourArg ++ "\n" ++ (show sizeArg) ++ "\n" ++ customStartArg ++ "\n" ++ hintArg ++ "\n" ++ diffArg ++ "\n")
                                              if (colourArg == "Black") || (colourArg == "black")
                                              then do gameLoop (GameState (board st) (turn st) (Just st) Black (hintsEnabled hintArg) (turn_time st) (difficultySetting diffArg))
                                              else do gameLoop (GameState (board st) (turn st) (Just st) White (hintsEnabled hintArg) (turn_time st) (difficultySetting diffArg))
                           -- Start game with default settings
                         | (length args) == 0 = do writeFile "autosave.txt" "Black\n8\nNo\nhints-off\nhard\n"
                                                   startGame Black 8 False False True 0 "" 
                           -- Reload a saved game
                         | (length args) == 1 = do if (head args) == "reload"
                                                   then do f <- doesFileExist "autosave.txt"               
                                                           if f then do myfile <- openFile "autosave.txt" ReadMode
                                                                        eof <- hIsEOF myfile
                                                                        if eof then putStrLn "No previously saved games"
                                                                               else do colourArg <- hGetLine myfile
                                                                                       sizeArg <- hGetLine myfile
                                                                                       customStartArg <- hGetLine myfile
                                                                                       hintArg <- hGetLine myfile
                                                                                       diffArg <- hGetLine myfile
                                                                                       passes <- hGetLine myfile
                                                                                       savedBoard <- hGetLine myfile
                                                                                       if (colourArg == "Black") || (colourArg == "black")
                                                                                       then startGame Black (getInt sizeArg) (customStartEnabled customStartArg) (hintsEnabled hintArg) (difficultySetting diffArg) (getInt passes) savedBoard
                                                                                       else startGame White (getInt sizeArg) (customStartEnabled customStartArg) (hintsEnabled hintArg) (difficultySetting diffArg) (getInt passes) savedBoard
                                                                         else putStrLn "No previously saved games"
                                                    else usage
                        -- Args given, evaluate and start game
                        | (length args) == 5 = do let colourArg = (head args)
                                                  let sizeArg = getInt ((\(x:y:xs) -> y) args)
                                                  let customStartArg = ((\(x:y:z:xs) -> z) args)
                                                  let hintArg = ((\(a:b:c:d:xs) -> d) args)
                                                  let diffArg = (last args)
                                                  writeFile "autosave.txt" (colourArg ++ "\n" ++ (show sizeArg) ++ "\n" ++ customStartArg ++ "\n" ++ hintArg ++ "\n" ++ diffArg ++ "\n")
                                                  if (sizeArg == 4) || (sizeArg == 6) || (sizeArg == 8) || (sizeArg == 10)
                                                  then if (colourArg == "Black") || (colourArg == "black")
                                                       then startGame Black sizeArg (customStartEnabled customStartArg) (hintsEnabled hintArg) (difficultySetting diffArg) 0 ""
                                                       else if (colourArg == "White") || (colourArg == "white")
                                                            then startGame White sizeArg (customStartEnabled customStartArg) (hintsEnabled hintArg) (difficultySetting diffArg) 0 ""
                                                            else usage 
                                                  else usage
                          -- Args not in correct format, print usage prompt
                        | otherwise =  usage

-- processes arg given for hints setting
-- defaults to hints turned off
hintsEnabled :: String -> Bool
hintsEnabled h | h == "hints-on" = True
               | otherwise = False

-- processes arg given for custom start setting
-- defaults to normal starting positions
customStartEnabled :: String -> Bool
customStartEnabled c | c == "custom-start" = True
                     | otherwise = False

-- process arg given for difficulty setting
-- defaults to hard difficulty
difficultySetting :: String -> Bool
difficultySetting d | d == "easy" = False
                    | otherwise = True

-- Creates initial gamestate and board (as appropriate to custom start settings) and starts the relevant version of the game loop
startGame :: Colour -> Int -> Bool -> Bool -> Bool -> Int -> String -> IO ()
startGame c s x h d saved_passes saved = do putStrLn "Controls:"
                                            putStrLn "When prompted to move, enter the co-ordinate e.g. D4"
                                            putStrLn "You can also type:"
                                            putStrLn "undo -- this will get rid of the most recent move"
                                            putStrLn "restart -- this will restart the game, and give you the choice to change settings"
                                            putStrLn "pause -- this will pause the game, and give you the choice to change settings"
                                            putStrLn "quit -- this will end the game, but give you the choice to save it first"
                                            putStrLn "What time limit would you like (seconds) (10-60)?"
                                            response <- getLine
                                            if (checkInteger response) == True
                                            then if (getInt response) > 60 || (getInt response) < 10
                                                 then do putStrLn "Invalid time!"
                                                         startGame c s x h d saved_passes saved
                                                 else do let time = (getInt response) * 1000
                                                         if saved /= ""
                                                         then do let grid = Map.fromList (read saved)
                                                                 let b = Board s saved_passes grid
                                                                 let st = GameState b Black Nothing c h time d
                                                                 gameLoop st
                                                         else do putStrLn ("X = Black")
                                                                 putStrLn ("O = White")
                                                 
                                                         if x
                                                         then do let emptyGrid = Map.fromList (zip [(x, y) | x <- [0..s], y <- [0..s]] (repeat Empty))
                                                                 let b = Board s 0 emptyGrid
                                                                 let st = GameState b Black Nothing c h time d
                                                                 customStartLoop 4 st
                                                         else gameLoop (initGameState c s h time d)
                                            else do putStrLn "Invalid time!"
                                                    startGame c s x h d saved_passes saved
 
-- Prints message outlining how to specify command line arguments properly
-- Exits, since message is only displayed if arguments are invalid
usage :: IO ()
usage = do putStrLn "Usage: "
           putStrLn "-> To reload saved game: program reload"
           putStrLn "-> To start new game: program [<colour> <board size(4/6/8/10)> <custom-start/normal-start> <hints-on/hints-off> <easy/hard>]"
           putStrLn "Run without arguments for default settings"
           return()
