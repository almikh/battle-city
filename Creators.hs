module Creators where

import Game
import System.Random
import Control.Monad
import Data.Array.IArray
import Data.IORef
import Data.List
import Data.Maybe
import KeyEvent

dt = 25
steps = 12
cellSize = 48

loadMap :: FilePath -> IO TGrid
loadMap file = do
  contents <- readFile file
  let (w : h : arr) = map (read) $ words contents
  return (listArray ((0, 0), (w-1, h-1)) arr)

getDir :: KeyboardEvent -> (Int, Int)
getDir key = case key of
  MoveLeftward -> (-1, 0)
  MoveRightward -> (1, 0)
  MoveForward -> (0, 1)
  MoveBackward -> (0, -1)
  otherwise -> (0, 0)

notImmortalObject :: Entity -> Bool
notImmortalObject o = (isObstacle o && isDestroyed o) || not (isObstacle o)

createTank :: (Int, Int) -> Entity
createTank pos = Tank {
    eId = 0,
    layer = 2,
    location = pos,
    direction = (0, 1),
    lifetime = 0,
    speed = 4,
    size = (cellSize, cellSize),
    recharges = 0,
    health = 1,
    sprite = [ "star:0", "star:1", "star:2", "star:3" ],
    rechargeTime = 1000,
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
    keyboardCallback _ No _ = return ()
    keyboardCallback state Fire id' = do
      game <- readIORef state
      let idea = lookup id' $ objects game
      when (isJust idea) $ do -- Во время этого шага таймера объекты могут измениться внутри forM_, поэтму пришлось обращаться к ним через индексы
        let obj = fromJust idea
        when (recharges obj <= 0) $ do
          let updState = updateObject game $ obj { recharges = rechargeTime obj }
          let bulletDir@(dx, dy) = direction obj
          let bullet = createBullet (0, 0) bulletDir
          let (x, y) = location obj
          let (ow, oh) = size obj
          let (bw, bh) = size bullet
          let bulletPos = (x + ((ow-bw) `div` 2) + (dx*ow) `div` 2, y + ((oh-bh) `div` 2) + (dy*oh) `div` 2)
          writeIORef state $ registryObject updState $ bullet { location = bulletPos }
    keyboardCallback state key id' = do
      game <- readIORef state
      let idea = lookup id' $ objects game
      when (isJust idea) $ do
        let obj = fromJust idea
        let spd = speed obj
        let newDir@(xdir, ydir) = getDir key
        let newLocation@(nx, ny) = changeLocation (location obj) (xdir*spd, ydir*spd)
        let newObj = obj { location = newLocation, direction = newDir }
        let otherObjs = filter (\(i, o) -> i /= eId obj) $ objects game
        let collision = filter (\(i, o) -> (getRect newObj) `isIntersect` (getRect o)) $ otherObjs
        let obstacles = filter (\(i, o) -> (isObstacle o && isImpassable o) || isTank o) collision
        if null obstacles && (containsRect screenRect (getRect newObj)) then
          writeIORef state $ updateObject game newObj
          else writeIORef state $ updateObject game $ obj { direction = newDir }

    timerCallback state 100 _ id' = do -- анимация
      game <- readIORef state
      let idea = lookup id' $ objects game
      when (isJust idea) $ do
        let obj = fromJust idea
        if (lifetime obj == 3) then
          writeIORef state $ updateObject game $ obj { lifetime = lifetime obj + 1, sprite = ["test"] }
          else do
            let (s:ss) = sprite obj
            writeIORef state $ updateObject game $ obj { lifetime = lifetime obj + 1, sprite = (ss ++ [s]) }
    timerCallback state 25 key id' = do
      game <- readIORef state
      let idea = lookup id' $ objects game
      when (isJust idea) $ do
        let obj = fromJust idea
        let newRecharge = (\r -> if r /= 0 then r - 25 else r) $ recharges obj
        writeIORef state $ updateObject game $ obj { recharges = newRecharge }
        (onKeyboardCallback obj) state key id'
    timerCallback state _ _ _ = return ()

createSlowTank :: (Int, Int) -> Entity
createSlowTank pos = tank {
    onKeyboardCallback = keyboardCallback,
    onTimerCallback = timerCallback
  }
  where
    tank = createTank pos
    keyboardCallback state key id' = return ()

    timerCallback state 100 _ id' = do -- анимация
      game <- readIORef state
      let idea = lookup id' $ objects game
      when (isJust idea) $ do
        let obj = fromJust idea
        if (lifetime obj == 3) then
          writeIORef state $ updateObject game $ obj { lifetime = lifetime obj + 1, sprite = ["test"] }
          else do
            let (s:ss) = sprite obj
            writeIORef state $ updateObject game $ obj { lifetime = lifetime obj + 1, sprite = (ss ++ [s]) }
    timerCallback state 25 key id' = do
      game <- readIORef state
      let idea = lookup id' $ objects game
      when (isJust idea) $ do
        let obj = fromJust idea
        let newRecharge = (\r -> if r /= 0 then r - 25 else r) $ recharges obj
        let spd = speed obj
        let otherObjs = filter (\(i, o) -> i /= id') $ objects game
        if direction obj == (0, 0) then do
          dir <- randomIO :: IO Int
          part <- randomIO :: IO Int
          let newDir@(xdir, ydir) = if part `mod` 2 == 0 then (dir `mod` 3 - 1, 0) else (0, dir `mod` 3 - 1)
          let newLocation@(nx, ny) = changeLocation (location obj) (xdir*spd, ydir*spd)
          let newObj = obj { location = newLocation, recharges = newRecharge }
          let collision = filter (\(i, o) -> (getRect newObj) `isIntersect` (getRect o)) $ otherObjs
          if (null collision && containsRect screenRect (getRect newObj)) then
            writeIORef state $ updateObject game $ obj { direction = newDir, recharges = newRecharge }
            else writeIORef state $ updateObject game $ obj { recharges = newRecharge }
          else do
            let newDir@(xdir, ydir) = direction obj
            let newLocation@(nx, ny) = changeLocation (location obj) (xdir*spd, ydir*spd)
            let newObj = obj { location = newLocation, recharges = newRecharge }
            let collision = filter (\(i, o) -> (getRect newObj) `isIntersect` (getRect o)) $ otherObjs
            if null collision && containsRect screenRect (getRect newObj) then
              writeIORef state $ updateObject game newObj
              else writeIORef state $ updateObject game $ obj { direction = (0, 0), recharges = newRecharge }
    timerCallback state _ key obj = return ()

createBullet :: (Int, Int) -> (Int, Int) -> Entity
createBullet pos dir = Bullet {
    eId = 0,
    layer = 3,
    location = pos,
    direction = dir,
    speed = 8,
    health = 1,
    size = (4, 4),
    sprite = ["bullet"],
    onCollisionCallback = (\_ objs -> return ()),
    onKeyboardCallback = (\_ key id' -> return ()),
    onTimerCallback = timerCallback
  }
  where
    timerCallback state 25 key id' = do
      game <- readIORef state
      let idea = lookup id' $ objects game
      when (isJust idea) $ do
        let obj = fromJust idea
        let spd = speed obj
        let (xdir, ydir) = direction obj
        let newLocation@(nx, ny) = changeLocation (location obj) (xdir*spd, ydir*spd)
        let newObj = obj { location = newLocation }
        let newRect = getRect newObj
        if not (containsRect screenRect newRect) then
          writeIORef state $ deleteObject (registryObject game $ createBoom (location newObj) (size newObj)) obj
          else do
            let otherObjs = filter (\(i, o) -> i /= id') $ objects game
            let collision = filter (\(_, o) -> newRect `isIntersect` (getRect o)) otherObjs
            let targets = filter (\(_, o) -> (isObstacle o && isImpassable o && not (isImmortal o)) || not (isObstacle o)) collision
            if not (null targets) then do
              let updated = map (\(_, o) -> o { health = (health o) - 1 }) $ filter (notImmortalObject . snd) targets
              let deleted = filter (\o -> health o <= 0) updated
              let newGame = deleteObjectsIf (updateObjects game updated) (\o -> health o <= 0)
              let addedExplosions = registryObjects newGame $ map (\o -> createBigBoom (location o) (size o)) deleted
              writeIORef state $ deleteObject addedExplosions obj
              when (null deleted) $ do
                game <- readIORef state
                writeIORef state $ registryObject game $ createBoom (location obj) (size obj)
                else
                  writeIORef state $ updateObject game newObj
    timerCallback state _ key _ = return ()

createObstacle :: (Int, Int) -> Entity
createObstacle pos = Obstacle {
    eId = 0,
    layer = 1,
    location = pos,
    isImpassable = False,
    isDestroyed = False,
    isImmortal = False,
    size = (cellSize `div` 2, cellSize `div` 2),
    health = 1,
    sprite = [""],
    onCollisionCallback = (\_ objs -> return ()),
    onKeyboardCallback = (\_ key obj -> return ()),
    onTimerCallback = timerCallback
  }
  where
  timerCallback state 200 _ id' = do -- анимация
    game <- readIORef state
    let idea = lookup id' $ objects game
    when (isJust idea) $ do
      let obj = fromJust idea
      let (s:ss) = sprite obj
      let newAnim = ss ++ [s]
      writeIORef state $ updateObject game $ obj { sprite = newAnim }
  timerCallback state _ key obj = return ()

createGrass :: (Int, Int) -> Entity
createGrass pos = obst {
    layer = 3,
    isImpassable = False,
    isDestroyed = False,
    sprite = ["grass"]
  }
  where obst = createObstacle pos

createWater :: (Int, Int) -> Entity
createWater pos = obst {
    isImpassable = True,
    isDestroyed = False,
    isImmortal = True,
    sprite = ["water"]
  }
  where obst = createObstacle pos

createArmor :: (Int, Int) -> Entity
createArmor pos = obst {
    isImpassable = True,
    isDestroyed = False,
    sprite = ["armor"]
  }
  where obst = createObstacle pos

createBrick :: (Int, Int) -> Entity
createBrick pos = obst {
    isImpassable = True,
    isDestroyed = True,
    sprite = ["brick"]
  }
  where obst = createObstacle pos

createBoom :: (Int, Int) -> (Int, Int) -> Entity
createBoom pos@(x, y) sz@(w, h) = Boom {
    eId = 0,
    layer = 3,
    size = (16, 16),
    location = (x + ((w-16) `div` 2), y + ((h-16) `div` 2)),
    health = 100500,
    sprite = ["boom:0", "boom:1", "boom:2"],
    onCollisionCallback = (\_ objs -> return ()),
    onKeyboardCallback = (\_ key obj -> return ()),
    onTimerCallback = timerCallback
  }
  where
  timerCallback state 100 _ id' = do -- анимация
    game <- readIORef state
    let idea = lookup id' $ objects game
    when (isJust idea) $ do
      let obj = fromJust idea
      let (s : other) = sprite obj
      if null other then
        writeIORef state $ deleteObject game obj
        else writeIORef state $ updateObject game $ obj { sprite = other }
  timerCallback state _ key obj = return ()

createBigBoom :: (Int, Int) -> (Int, Int) -> Entity
createBigBoom pos@(x, y) sz@(w, h) = boom {
    location = (x + ((w-32) `div` 2), y + ((h-32) `div` 2)),
    size = (32, 32),
    sprite = ["bigboom:0", "bigboom:1"]
  }
  where boom = createBoom pos sz

createStandart :: (Int, Int) -> Entity
createStandart pos = Boom {
    eId = 0,
    layer = 1,
    size = (cellSize, cellSize),
    location = pos,
    health = 1,
    sprite = ["standart", "fall_standart"],
    onCollisionCallback = (\_ objs -> return ()),
    onKeyboardCallback = (\_ key obj -> return ()),
    onTimerCallback = (\_ _ key obj -> return ())
  }
