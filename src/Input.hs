module Input where

import Board
import Data.Char

-- Given an input string, convert it to a board coordinate as a pair of Ints
-- e.g. getCoord "D4" => (3,3) since coordinates are 0-based internally.
--   or getCoord "F2" => (5,1)
getCoord :: String -> (Int, Int)
getCoord (x:y) = (letterToInt x, getInt y)

getInt :: [Char] -> Int
getInt x = read x

letterToInt :: Char -> Int 
letterToInt c = (fromEnum c) - 65

intToLetter :: Int -> [Char]
intToLetter x = [toEnum (x + 65)::Char]

checkInteger :: [Char] -> Bool
checkInteger xs = do let a = length xs
                     let b = length [x | x <- xs, (isDigit x == True)]
                     if a - b > 0 then False
                     else True

