module Creators where

import Game
import System.Random
import Control.Monad
import Data.Array.IArray
import Data.IORef
import Data.List
import KeyEvent

dt = 25
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
    eId = 0,
    layer = 2,
    location = pos,
    direction = (0, 1),
    speed = (0, 0),
    diff = (0, 0),
    size = (1, 1),
    recharges = 0,
    health = 1,
    sprite = "test",
    rechargeTime = 1500,
    onCollisionCallback = (\_ objs -> return ()), -- никак не реагирует
    onKeyboardCallback = (\_ key obj -> return ()), -- никак не реагирует
    onTimerCallback = (\_ _ key obj -> return ())
  }

createHero :: (Int, Int) -> Entity
createHero pos = tank {
    onKeyboardCallback = keyboardCallback,
    onTimerCallback = timerCallback
  }
  where
    tank = createTank pos
    keyboardCallback _ No obj = return ()
    keyboardCallback state Fire obj = do
      when (recharges obj <= 0) $ do
        game <- readIORef state
        let updState = updateObject game $ obj { recharges = 1500 }
        let bulletPos = location obj
        let bulletSpeed = direction obj
        writeIORef state $ registryObject updState $ createBullet bulletPos bulletSpeed
      return ()
    keyboardCallback state key obj = do
      game <- readIORef state
      let grid' = grid game
      let step = (cellSize `div` steps)
      let newSpeed@(xspeed, yspeed) = getSpeed key
      let newLocation@(nx, ny) = changeLocation (location obj) newSpeed
      let newDiff = (xspeed*(step-cellSize), yspeed*(step-cellSize))
      let isValidCell = (isValidInd grid' nx ny)
      let notWillMove = (grid' ! newLocation /= 0) && isImpassableObj (getObjectFromCoord game newLocation)
      if not isValidCell || notWillMove then
        writeIORef state $ updateObject game $ obj { direction = newSpeed }
      else when (speed obj == (0,0)) $ do
        if newSpeed /= (0, 0) && direction obj /= newSpeed then -- поменяем ориентацию сперва (мб танк просто хочет развернуться на месте)
          writeIORef state $ updateObject game $ obj { direction = newSpeed }
          else do
            let newGrid = (grid game) // [(location obj, 0), (newLocation, 1)]
            let newGame = game { grid = newGrid }
            writeIORef state $ updateObject newGame $ obj { location = newLocation, direction = newSpeed, speed = newSpeed, diff = newDiff }

    timerCallback state 25 key obj = do
      game <- readIORef state
      let (dx, dy) = diff obj
      let (xspeed, yspeed) = speed obj
      let step = (cellSize `div` steps)
      let newRecharge = (\r -> if r /= 0 then r - 25 else r) $ recharges obj
      let newDiff = (dx + xspeed*step, dy + yspeed*step)
      if diff obj /= (0, 0) then do
        writeIORef state $ updateObject game $ obj { recharges = newRecharge, diff = newDiff }
        when (newDiff == (0, 0)) $ (onKeyboardCallback obj) state key $ obj { recharges = newRecharge, diff = newDiff }
        else do
          writeIORef state $ updateObject game $ obj { recharges = newRecharge, speed = (0, 0) }
          (onKeyboardCallback obj) state key $ obj { recharges = newRecharge, speed = (0, 0) }
    timerCallback state _ key obj = return ()

createBullet :: (Int, Int) -> (Int, Int) -> Entity
createBullet pos spd = Bullet {
    eId = 0,
    layer = 3,
    diff = (0, 0),
    location = pos,
    speed = spd,
    sprite = "bullet",
    onCollisionCallback = (\_ objs -> return ()),
    onKeyboardCallback = (\_ key obj -> return ()),
    onTimerCallback = timerCallback
  }
  where
    timerCallback state 15 key obj = do
      game <- readIORef state
      let step = (cellSize `div` steps)
      let (xspeed, yspeed) = speed obj
      if diff obj /= (0, 0) then do
        let (dx, dy) = diff obj
        let newDiff = (dx + xspeed*step, dy + yspeed*step)
        writeIORef state $ updateObject game $ obj { diff = newDiff }
        else do
          let grid' = grid game
          let newLocation@(nx, ny) = changeLocation (location obj) (speed obj)
          let newDiff = (xspeed*(step-cellSize), yspeed*(step-cellSize))
          if not (isValidInd grid' nx ny) then do
            putStrLn "deleted bullet"
            writeIORef state $ deleteObject game obj
            else writeIORef state $ updateObject game $ obj { location = newLocation, diff = newDiff }
    timerCallback state _ key obj = return ()

createObstacle :: (Int, Int) -> Entity
createObstacle pos = Obstacle {
    eId = 0,
    layer = 1,
    location = pos,
    isImpassable = False,
    isDestroyed = False,
    health = 1,
    sprite = "",
    onCollisionCallback = (\_ objs -> return ()),
    onKeyboardCallback = (\_ key obj -> return ()),
    onTimerCallback = (\_ _ key obj -> return ())
  }

createGrass :: (Int, Int) -> Entity
createGrass pos = obst {
    layer = 3,
    isImpassable = False,
    isDestroyed = False,
    sprite = "grass"
  }
  where obst = createObstacle pos

createWater :: (Int, Int) -> Entity
createWater pos = obst {
    isImpassable = True,
    isDestroyed = False,
    sprite = "water"
  }
  where obst = createObstacle pos

createArmor :: (Int, Int) -> Entity
createArmor pos = obst {
    isImpassable = True,
    isDestroyed = False,
    sprite = "armor"
  }
  where obst = createObstacle pos

createBrick :: (Int, Int) -> Entity
createBrick pos = obst {
    isImpassable = True,
    isDestroyed = True,
    health = 4,
    sprite = "brick"
  }
  where obst = createObstacle pos
