-- *** INSTRUCTIONS: *** --
  -- Open your TicHaskToe repository in Gitpod
  -- Create a file `State.hs` in the `solutions` directory and paste the code below.
  -- Open the `TicHaskToe.cabal` file in the root directory,
    -- find the `executable TicHaskToe` stanza,and make the following changes:
      -- 1. Add `, State` below `, A5` in the `other-modules:` section
      -- 2. Add `, mtl` below `, random` in the `build-depends:` section
  -- To test the code:
    -- 1. Run `cabal repl` in the terminal and then `:l State`
    -- 2. Enter `randomGame`
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use tuple-section" #-}
{-# HLINT ignore "Eta reduce" #-}

module State where

import A1
import A2
import A4
import A5

import System.Random (StdGen, randomR, newStdGen)
import Control.Monad.State

-- define a record type to hold the state
data Game = Game {
    activePlayer :: Player
  , status       :: GameState
  , board        :: Board
  , generator    :: StdGen
  }

-- create an initial Game value using a random generator
initialState :: StdGen -> Game
initialState gen = Game {
    activePlayer = X
  , status       = InProgress
  , board        = _EMPTY_BOARD_
  , generator    = gen
  }

-- game loop
playGame :: State Game ()
playGame = do
  Game ap _ b gen <- get
  let opens      = getOpenMoves b
      (i, gen')  = randomR (0, length opens - 1) gen
      move       = opens !! i
      (stat, b') = playMove ap b move
  -- update the state
  put $ Game { activePlayer = switchPlayer ap
             , status       = stat
             , board        = b'
             , generator    = gen'
             }
  -- play again if game isn't over (otherwise return ())
  when (stat == InProgress) playGame

-- alternative version:
updateGame :: Game -> Game
updateGame (Game ap _ b gen) = Game
  { activePlayer = switchPlayer ap
  , status       = stat
  , board        = b'
  , generator    = gen'
  }
  where
    opens      = getOpenMoves b
    (i, gen')  = randomR (0, length opens - 1) gen
    move       = opens !! i
    (stat, b') = playMove ap b move

playGame' :: State Game ()
playGame' = do
  modify updateGame
  stat <- gets status
  when (stat == InProgress) playGame'

randomGame :: IO ()
randomGame = do
  game <- initialState <$> newStdGen -- get a random generator and create initial game state
  let (_, game') = runState playGame' game -- call runner to get handler and apply it to initial state
  -- display result
  printBoard $ board game'
  putStrLn . showGameState $ status game'

-- *** HELPERS TO IDENTIFY INDICES OF OPEN SQUARES *** --
getOpenMoves :: Board -> [Move]
getOpenMoves b = concat $ go 0 [] b
  where
    go _ ms []     = ms
    go i ms (r:rs) = go (i + 1) (map (\j -> (i, j)) (getOpenSquares r) : ms) rs

getOpenSquares :: Row -> [Int]
getOpenSquares r = go 0 [] r
  where
    go _ os []       = os
    go j os (sq:sqs)
      | sq == E      = go (j + 1) (j:os) sqs
      | otherwise    = go (j + 1) os     sqs