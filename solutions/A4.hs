module A4 where

import A1
import A2
import A3 (putSquare)
import Data.List (transpose)

-- *** Assignment 4-1 *** --

-- Q#01
_HEADER_ :: String
_HEADER_ = ' ' : formatLine (map show _RANGE_)

formatRow :: Row -> String
formatRow r = formatLine $ map showSquare r

formatRows :: [Row] -> [String]
formatRows rs = map formatRow rs

--Q#02
--rewrite formatRows using a lambda expression instead of formatRow
formatRowsLambda :: [Row] -> [String]
formatRowsLambda rs = map (\r -> formatLine (map showSquare r)) rs

-- Q#03
dropFirstCol :: Board -> Board
dropFirstCol b = map tail b

dropLastCol :: Board -> Board
dropLastCol b = map init b

-- Q#04
getDiag1 :: Board -> Line
getDiag1 []       = []
getDiag1 (r : rs) = head r : getDiag1 (dropFirstCol rs)

getDiag2 :: Board -> Line
getDiag2 []       = []
getDiag2 (r : rs) = last r : getDiag2 (dropLastCol rs)

getAllLines :: Board -> [Line]
getAllLines b = concat [b, transpose b, [getDiag1 b, getDiag2 b]]

-- Q#06
isWinningLine_ :: Player -> Line -> Bool
isWinningLine_ p l = not (null l) && null (filter (/= p) l)

-- *** Assignment 4-2 *** --

-- Q#07
isWinningLine :: Player -> Line -> Bool
isWinningLine p l = not (null l) && foldr (\s acc -> acc && s == p) True l

-- Q#08
hasWon :: Player -> Board -> Bool
hasWon p b = foldr (\l acc -> acc || isWinningLine p l) False $ getAllLines b

_X_WIN_ = [ [X, O, O]
          , [O, X, O]
          , [O, O, X]
          ]

_O_WIN_ = [ [O, X, O]
          , [X, X, O]
          , [X, O, O]
          ]

-- Q#09
getGameState :: Board -> GameState
getGameState b
  | hasWon X b = XWon
  | hasWon O b = OWon
  | isTied b   = Tie
  | otherwise  = InProgress

playMove :: Player -> Board -> Move -> (GameState, Board)
playMove p b m = (getGameState b', b')
  where
    b' = putSquare p b m

-- Q#10
prependRowIndices :: [String] -> [String]
prependRowIndices ss = zipWith (:) ['A' .. ] ss

-- Q#11
formatBoard :: Board -> String
formatBoard b = unlines . (_HEADER_ :) . prependRowIndices $ formatRows b