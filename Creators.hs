module Creators where

import Game
import System.Random
import Control.Monad
import Data.Array.IArray
import Data.IORef
import KeyEvent

steps = 12
cellSize = 48

getSpeed :: KeyboardEvent -> TSpeed
getSpeed key = case key of
  MoveLeftward -> (-1, 0)
  MoveRightward -> (1, 0)
  MoveForward -> (0, 1)
  MoveBackward -> (0, -1)
  otherwise -> (0, 0)


createTank :: (Int, Int) -> Entity
createTank pos = Tank {
    location = pos,
    direction = (0, 0),
    speed = (0, 0),
    diff = (0, 0),
    size = (1, 1),
    health = 1,
    sprite = "test",
    onCollisionCallback = (\_ objs -> return ()), -- никак не реагирует
    onKeyboardCallback = (\_ key obj -> return ()), -- никак не реагирует
    onTimerCallback = (\_ key obj -> return ())
  }

createHero :: (Int, Int) -> Entity
createHero pos = tank {
    onKeyboardCallback = keyboardCallback,
    onTimerCallback = timerCallback
  }
  where
    tank = createTank pos
    keyboardCallback _ No obj = return ()
    keyboardCallback state key obj = do
      game <- readIORef state
      let grid' = grid game
      let step = (cellSize `div` steps)
      let newSpeed@(xspeed, yspeed) = getSpeed key
      let newLocation@(nx, ny) = changeLocation (location obj) newSpeed
      let newDiff = (xspeed*(step-cellSize), yspeed*(step-cellSize))
      if (not (isValidInd grid' nx ny) || (grid' ! newLocation) /= 0) then
        writeIORef state $ updateObject game 1 (obj { direction = newSpeed })
      else
        when (speed obj == (0,0)) $ do
          let newGrid = (grid game) // [(location obj, 0), (newLocation, 1)]
          let newGame = game { grid = newGrid }
          writeIORef state $ updateObject newGame 1 (obj { location = newLocation, direction = newSpeed, speed = newSpeed, diff = newDiff })

    timerCallback state key obj = do
      game <- readIORef state
      let (xspeed, yspeed) = speed obj
      let step = (cellSize `div` steps)
      let (dx, dy) = diff obj
      let newDiff = (dx + xspeed*step, dy + yspeed*step)
      if diff obj /= (0, 0) then do
        writeIORef state $ updateObject game 1 $ obj { diff = newDiff }
        when (newDiff == (0, 0)) $ (onKeyboardCallback obj) state key $ obj { diff = newDiff }
        else do
          writeIORef state $ updateObject game 1 $ obj { speed = (0, 0) }
          (onKeyboardCallback obj) state key $ obj { speed = (0, 0) }
