module A5 where

import A1
import A2
import A3
import A4

import System.Random.Stateful (globalStdGen, uniformM)
import Control.Monad (when)

-- *** Assignment 5-1 *** --

-- Q#01

printBoard :: Board -> IO ()
printBoard b = putStrLn (formatBoard b)

-- Q#02
_LOGO_PATH_ :: FilePath
_LOGO_PATH_ = "./assets/logo.txt"

printLogo :: IO ()
printLogo = readFile _LOGO_PATH_ >>= putStrLn

-- Q#03
_RANDOM_BOOL_ :: IO Bool
_RANDOM_BOOL_ = uniformM globalStdGen

firstPlayer :: IO Player
firstPlayer = _RANDOM_BOOL_ >>= (\b -> return (getFirstPlayer b))

-- Q#04
getMove :: Board -> IO Move
getMove b = getLine >>= go
  where
    go :: String -> IO Move
    go i
      | isValidMove b $ stringToMove i = return $ stringToMove i
      | otherwise = putStrLn "Invalid move! Try again" >> getMove b

-- Q#05
play :: Board -> Player -> IO ()
play b p =
  when _DISPLAY_LOGO_ printLogo >>
  printBoard b >>
  putStrLn (promptPlayer p) >>
  getMove b >>= go
  where
    go :: Move -> IO ()
    go m = case playMove p b m of
      (InProgress, b') -> play b' (switchPlayer p)
      (gs, b') -> printBoard b' >> putStrLn (showGameState gs)

-- *** Assignment 5-2 *** --

-- Q#07
printLogoDo :: IO ()
printLogoDo = do
    logo <- readFile _LOGO_PATH_
    putStrLn logo

-- Q#08
firstPlayerDo :: IO Player
firstPlayerDo = do
    bool <- _RANDOM_BOOL_
    return (getFirstPlayer bool)

-- Q#09
getMoveDo :: Board -> IO Move
getMoveDo b = do
  s <- getLine
  let m = stringToMove s
  if isValidMove b m
    then return m
    else do
      putStrLn "Invalid move! Try again"
      getMoveDo b

-- Q#10
playDo :: Board -> Player -> IO ()
playDo b p = do
  when _DISPLAY_LOGO_ printLogo
  printBoard b
  putStrLn (promptPlayer p)
  m <- getMoveDo b
  case playMove p b m of
    (InProgress, b') -> play b' (switchPlayer p)
    (gs, b') -> do
      printBoard b'
      putStrLn $ showGameState gs