module Creators where

import Game
import System.Random
import Control.Monad
import Data.Array.IArray
import Data.IORef
import Data.List
import Data.Maybe
import KeyEvent

steps = 12
cellSize = 32
screenWidth :: Int
screenWidth = 32*13

screenHeight :: Int
screenHeight = 32*13

respawnTime :: Int
respawnTime = 25000

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

rotateClockwise :: (Int, Int) -> (Int, Int)
rotateClockwise (1, 0) = (0, -1)
rotateClockwise (0, 1) = (1, 0)
rotateClockwise (-1, 0) = (0, 1)
rotateClockwise (0, -1) = (-1, 0)
rotateClockwise _ = (0, 0)

rotateAntiClockwise :: (Int, Int) -> (Int, Int)
rotateAntiClockwise (1, 0) = (0, 1)
rotateAntiClockwise (0, 1) = (-1, 0)
rotateAntiClockwise (-1, 0) = (0, -1)
rotateAntiClockwise (0, -1) = (1, 0)
rotateAntiClockwise _ = (0, 0)

notImmortalObject :: Entity -> Bool
notImmortalObject o
  | isRespawnPoint o = False
  | isObstacle o = isDestroyed o
  | isTank o = invulnerable o == 0
  | otherwise = True

isCollidingObject :: Entity -> Bool
isCollidingObject o
  | isRespawnPoint o = False
  | isBoom o = False
  | isObstacle o = isImpassable o -- && not (isImmortal o)
  | otherwise = True

notEffect :: Entity -> Bool
notEffect o
  | isBoom o = False
  | isBullet o = False
  | otherwise = True

checkInvulnerable :: Entity -> IO Entity
checkInvulnerable o
  | isTank o = do
    if (invulnerable o == 1) then
      return $ o { invulnerable = invulnerable o - 1, spritesEffects = [] }
      else
        if (invulnerable o > 1) then do
          let effects = spritesEffects o
              newSpritesEffects = if null effects then [] else (tail effects ++ [head effects])
          return $ o { invulnerable = invulnerable o - 1, spritesEffects = newSpritesEffects }
          else return o
  | otherwise = return o

createTank :: (Int, Int) -> Entity
createTank pos = Tank {
    eId = 0,
    layer = 2,
    location = pos,
    direction = (0, 1),
    lifetime = 0,
    duration = 0,
    side = 0,
    invulnerable = 0,
    speed = cellSize `div` 16,
    size = (cellSize, cellSize),
    recharges = 0,
    health = 1,
    targetDt = 0,
    sprite = [ "star:0", "star:1", "star:2", "star:3" ],
    rechargeTime = 1000,
    targetSprites = [],
    spritesEffects = [],
    onKeyboardCallback = (\_ _ _ -> return ()), -- никак не реагирует
    onTimerCallback = timerCallback
  }
  where
    tank = createTank pos
    timerCallback state 100 id' = do -- анимация
      game <- readIORef state
      let idea = lookup id' $ objects game
      when (isJust idea) $ do
        let obj = fromJust idea
        if (lifetime obj == 6) then
          writeIORef state $ updateObject game $ obj { lifetime = lifetime obj + 1, sprite = targetSprites obj }
          else do
            let (s:ss) = sprite obj
            writeIORef state $ updateObject game $ obj { lifetime = lifetime obj + 1, sprite = (ss ++ [s]) }
    timerCallback state dt id' = do
      game <- readIORef state
      let idea = lookup id' $ objects game
      when (isJust idea && (lifetime (fromJust idea) >= 6) && (dt == targetDt (fromJust idea))) $ do
        let obj = fromJust idea
        -- проверка выстрела
        num <- randomIO :: IO Int
        when (num `mod` 32 == 0 && recharges obj <= 0) $ do
          let updState = updateObject game $ obj { recharges = rechargeTime obj }
          let bulletDir@(dx, dy) = direction obj
          let bullet = createBullet 0 (0, 0) bulletDir
          let (x, y) = location obj
          let (ow, oh) = size obj
          let (bw, bh) = size bullet
          let bulletPos = (x + ((ow-bw) `div` 2) + (dx*ow) `div` 2, y + ((oh-bh) `div` 2) + (dy*oh) `div` 2)
          writeIORef state $ registryObject updState $ bullet { location = bulletPos }
        -- ИИ для движения
        game <- readIORef state
        let Just obj = lookup id' $ objects game
        let spd = speed obj
        let others = filter (\(i, o) -> i /= id' && notEffect o) $ objects game
        let newDuration = duration obj + dt
        let newRecharge = (\r -> if r /= 0 then r - dt else r) $ recharges obj
        let changeDirection = \o -> do
            num <- randomIO :: IO Int
            part <- randomIO :: IO Int
            let sdir = if num `mod` 2 == 0 then -1 else 1
            let periodDuration = respawnTime `div` 8
            if duration o < periodDuration then do -- двигаться случайно
              let newDir@(xdir, ydir) = if part `mod` 2 == 0 then (sdir, 0) else (0, sdir)
              writeIORef state $ updateObject game $ o { duration = newDuration, direction = newDir, recharges = newRecharge }
              else if duration obj < periodDuration*2 then do -- двигаться к игроку
                let newDir@(xdir, ydir) = if part `mod` 2 == 0 then (sdir, 0) else (0, sdir)
                writeIORef state $ updateObject game $ o { duration = newDuration, direction = newDir, recharges = newRecharge }
                return ()
                else do -- двигаться к штабу
                  let newDir@(xdir, ydir) = if part `mod` 2 == 0 then (sdir, 0) else (0, sdir)
                  writeIORef state $ updateObject game $ o { duration = newDuration, direction = newDir, recharges = newRecharge }
                  return ()
        let changeDirectionCommand = \o -> do
            num <- randomIO :: IO Int
            case num `mod` 3 of
              0 -> changeDirection o
              1 -> do -- rotate clockwise
                let newDir = rotateClockwise $ direction o
                writeIORef state $ updateObject game $ o { duration = newDuration, direction = newDir, recharges = newRecharge }
              otherwise -> do -- rotate anticlockwise
                let newDir = rotateAntiClockwise $ direction o
                writeIORef state $ updateObject game $ o { duration = newDuration, direction = newDir, recharges = newRecharge }
        let checkCollision = \o -> do
            let coord@(x, y) = location o
            num <- randomIO :: IO Int
            if (x `mod` cellSize == 0) && (y `mod` cellSize == 0) && (num `mod` 8 == 0) then do
              changeDirection o
              else do
                num <- randomIO :: IO Int
                let dir@(xdir, ydir) = direction o
                    newLocation@(nx, ny) = changeLocation coord (xdir*spd, ydir*spd)
                    newObj = o { location = newLocation }
                    collision = filter (\(_, e) -> isCollidingObject e && (getRect newObj) `isIntersect` (getRect e)) $ others
                    flag = containsRect screenRect (getRect newObj) && null collision
                if not flag && (num `mod` 16 == 0) then do
                  if (x `mod` cellSize == 0) && (y `mod` cellSize == 0) then do -- invert direction
                    writeIORef state $ updateObject game $ o { duration = newDuration, direction = (-xdir, -ydir), recharges = newRecharge }
                    else do
                      changeDirectionCommand o
                  else do
                    writeIORef state $ updateObject game $ o { duration = newDuration, recharges = newRecharge }

        checkCollision obj
        game <- readIORef state
        -- двигаем его
        let Just obj = lookup id' $ objects game
            dir@(xdir, ydir) = direction obj
            newLocation@(nx, ny) = changeLocation (location obj) (xdir*spd, ydir*spd)
            newObj = obj { duration = newDuration, location = newLocation, recharges = newRecharge }
            collision = filter (\(_, e) -> isCollidingObject e && (getRect newObj) `isIntersect` (getRect e)) $ others
            flag = containsRect screenRect (getRect newObj) && null collision
        if flag then
          writeIORef state $ updateObject game newObj
          else
            writeIORef state $ updateObject game $ obj { duration = newDuration, recharges = newRecharge }

createHero :: (Int, Int) -> Entity
createHero pos = tank {
    side = 1,
    targetDt = averageDt,
    targetSprites = ["hero0", "hero1"],
    onKeyboardCallback = keyboardCallback,
    onTimerCallback = timerCallback
  }
  where
    tank = createTank pos
    keyboardCallback _ No _ = return ()
    keyboardCallback state Fire id' = do
      game <- readIORef state
      let idea = lookup id' $ objects game
      when (isJust idea && (lifetime (fromJust idea) >= 3)) $ do
        -- Во время этого шага таймера объекты могут измениться внутри forM_, поэтму пришлось обращаться к ним через индексы
        let obj = fromJust idea
        when (recharges obj <= 0) $ do
          let updState = updateObject game $ obj { recharges = rechargeTime obj }
          let bulletDir@(dx, dy) = direction obj
          let bullet = createBullet 1 (0, 0) bulletDir
          let (x, y) = location obj
          let (ow, oh) = size obj
          let (bw, bh) = size bullet
          let bulletPos = (x + ((ow-bw) `div` 2) + (dx*ow) `div` 2, y + ((oh-bh) `div` 2) + (dy*oh) `div` 2)
          writeIORef state $ registryObject updState $ bullet { location = bulletPos }
    keyboardCallback state key id' = do
      game <- readIORef state
      let idea = lookup id' $ objects game
      when (isJust idea && (lifetime (fromJust idea) >= 3)) $ do
        let obj = fromJust idea
        let spd = speed obj
        let newDir@(xdir, ydir) = getDir key
        let newLocation@(nx, ny) = changeLocation (location obj) (xdir*spd, ydir*spd)
        let newObj = obj { location = newLocation, direction = newDir }
        let others = filter (\(i, o) -> i /= eId obj && notEffect o) $ objects game
        let obstacles = filter (\(i, o) -> isCollidingObject o && (getRect newObj) `isIntersect` (getRect o)) others
        if null obstacles && (containsRect screenRect (getRect newObj)) then
          writeIORef state $ updateObject game newObj
          else writeIORef state $ updateObject game $ obj { direction = newDir }

    timerCallback state 100 id' = do -- анимация
      game <- readIORef state
      let idea = lookup id' $ objects game
      when (isJust idea) $ do
        obj <- checkInvulnerable $ fromJust idea

        if (lifetime obj == 6) then do
          let newLife = lifetime obj + 1
              ts = targetSprites obj
              se = ["field:0", "field:1"]
          writeIORef state $ updateObject game $ obj { lifetime = newLife, sprite = ts, invulnerable = 64, spritesEffects = se }
          else do
            let (s:ss) = sprite obj
            writeIORef state $ updateObject game $ obj { lifetime = lifetime obj + 1, sprite = (ss ++ [s]) }
    timerCallback state dt id' = do
      game <- readIORef state
      let idea = lookup id' $ objects game
      when (isJust idea && (lifetime (fromJust idea) >= 6) && (dt == targetDt (fromJust idea))) $ do
        forM_ (keys game) $ \key -> do -- обработаем события клавиатуры
          (onKeyboardCallback (fromJust idea)) state key id'

        game <- readIORef state
        let Just obj = lookup id' $ objects game
        let newRecharge = (\r -> if r /= 0 then r - dt else r) $ recharges obj
        writeIORef state $ updateObject game $ obj { recharges = newRecharge }

createSlowTank :: (Int, Int) -> Entity
createSlowTank pos = tank {
    side = 0,
    health = 4,
    targetDt = slowDt,
    targetSprites = ["slow0", "slow1"],
    onKeyboardCallback = (\_ _ _ -> return ())
  }
  where
    tank = createTank pos

createAvTank :: (Int, Int) -> Entity
createAvTank pos = tank {
    side = 0,
    targetDt = averageDt,
    targetSprites = ["average0", "average1"],
    onKeyboardCallback = (\_ _ _ -> return ())
  }
  where
    tank = createTank pos

createFastTank :: (Int, Int) -> Entity
createFastTank pos = tank {
    side = 0,
    targetDt = fastDt,
    targetSprites = ["fast0", "fast1"],
    onKeyboardCallback = (\_ _ _ -> return ())
  }
  where
    tank = createTank pos

createBullet :: Int -> (Int, Int) -> (Int, Int) -> Entity
createBullet sd pos dir = Bullet {
    eId = 0,
    layer = 3,
    location = pos,
    direction = dir,
    speed = cellSize `div` 16,
    side = sd,
    health = 1,
    size = (5, 5),
    sprite = ["bullet"],
    onTimerCallback = timerCallback
  }
  where
    timerCallback state dt id' = do
      when (dt==fastDt) $ do
        game <- readIORef state
        let idea = lookup id' $ objects game
        when (isJust idea) $ do
          let obj = fromJust idea
              objSize@(sx, sy) = size obj
              objCoord@(lx, ly) = location obj
              bullSide = side obj
              spd = speed obj
              (xdir, ydir) = direction obj
              newLocation@(nx, ny) = changeLocation objCoord (xdir*spd, ydir*spd)
              newObj = obj { location = newLocation }
              newRect = getRect newObj
          if not (containsRect screenRect newRect) then do
            writeIORef state $ deleteObject (registryObject game $ createBoom objCoord) obj
            else do
              let others = filter (\(i, o) -> i /= id' && (not (isTank o) || side o /= bullSide)) $ objects game
                  targets = filter (\(_, o) -> isCollidingObject o && (not (isObstacle o) || not (isImmortal o)) && newRect `isIntersect` (getRect o)) others
              if not (null targets) then do
                let eagle = filter (isStandart . snd) targets
                    updated = map (\(_, o) -> o { health = (health o) - 1 }) $ filter (\(_, o) -> not (isStandart o) && notImmortalObject o) targets
                    deleted = filter (\o -> health o <= 0) updated
                    existsHero = not $ null $ filter (\o -> isTank o && side o == 1) deleted
                    newLifes = if existsHero then (lifes game - 1) else lifes game
                    isGameOver = (newLifes <= 0) || (not $ null eagle)
                    booms = map (\o -> createBigBoom (location o) (size o)) deleted
                if isGameOver then
                  if not $ null eagle then do
                    let newEagle = (snd (head eagle)) { health = 100500, sprite = ["fall_standart"] }
                        withUpdEagle = updateObject game newEagle
                        newGame = deleteObjectsIf (updateObjects withUpdEagle updated) (\o -> eId o == id' || health o <= 0)
                    writeIORef state $ registryObjects newGame booms
                    else do
                      let eagle = snd $ head $ filter (isStandart . snd) others
                      writeIORef state $ updateObject game $ eagle { health = 100500, sprite = ["fall_standart"] }
                  else if existsHero then do
                    let newGame = deleteObjectsIf (updateObjects game updated) (\o -> eId o == id' || health o <= 0)
                        withNewHero = registryObject (newGame { lifes = newLifes }) $ createHero (4*cellSize, 0*cellSize)
                    writeIORef state $ registryObjects withNewHero booms
                    else do
                      let newGame = deleteObjectsIf (updateObjects game updated) (\o -> eId o == id' || health o <= 0)
                      writeIORef state $ registryObjects newGame booms

                when (null deleted) $ do
                  game <- readIORef state
                  writeIORef state $ registryObject (deleteObject game obj) $ createBoom (location obj)
                  return ()
              else
                writeIORef state $ updateObject game newObj

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
    onTimerCallback = timerCallback
  }
  where
  timerCallback state 100 id' = do -- анимация
    game <- readIORef state
    let idea = lookup id' $ objects game
    when (isJust idea) $ do
      let obj = fromJust idea
      let (s:ss) = sprite obj
      let newAnim = ss ++ [s]
      writeIORef state $ updateObject game $ obj { sprite = newAnim }
  timerCallback state _ obj = return ()

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
    sprite = ["water:0", "water:1"],
    onTimerCallback = timerCallback
  }
  where
    obst = createObstacle pos
    timerCallback state 600 id' = do -- анимация
      game <- readIORef state
      let idea = lookup id' $ objects game
      when (isJust idea) $ do
        let obj = fromJust idea
        let (s:ss) = sprite obj
        let newAnim = ss ++ [s]
        writeIORef state $ updateObject game $ obj { sprite = newAnim }
    timerCallback _ _ _ = return ()

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

createBoom :: (Int, Int) -> Entity
createBoom pos@(x, y) = Boom {
    eId = 0,
    layer = 3,
    size = (16, 16),
    location = (x, y),
    health = 100500,
    sprite = ["boom:0", "boom:1", "boom:2"],
    onTimerCallback = timerCallback
  }
  where
  timerCallback state 100 id' = do -- анимация
    game <- readIORef state
    let idea = lookup id' $ objects game
    when (isJust idea) $ do
      let obj = fromJust idea
      let (s : other) = sprite obj
      if null other then
        writeIORef state $ deleteObject game obj
        else writeIORef state $ updateObject game $ obj { sprite = other }
  timerCallback state _ obj = return ()

createBigBoom :: (Int, Int) -> (Int, Int) -> Entity
createBigBoom pos@(x, y) sz@(w, h) = boom {
    location = (x, y),
    size = (w, h),
    sprite = ["bigboom:0", "bigboom:1"]
  }
  where boom = createBoom pos

createStandart :: (Int, Int) -> Entity
createStandart pos = Standart {
    eId = 0,
    layer = 1,
    size = (cellSize, cellSize),
    location = pos,
    health = 1,
    sprite = ["standart", "fall_standart"],
    onTimerCallback = (\_ _ obj -> return ())
  }

createRespawnPoint :: (Int, Int) -> Entity
createRespawnPoint pos = RespawnPoint {
    eId = 0,
    layer = 1,
    health = 1,
    duration = 0,
    size = (cellSize, cellSize),
    location = pos,
    onTimerCallback = timerCallback
  }
  where
  timerCallback state 100 id' = do -- анимация
    game <- readIORef state
    -- num <- randomIO :: IO Int
    let idea = lookup id' $ objects game
    when (isJust idea) $ do
      let obj = fromJust idea
          oldDuration = duration obj
      if (enemyTanks game > 0) && (oldDuration >= respawnTime) then do
        let oldEnemyTanks = enemyTanks game
        let newGame = registryObject (game { enemyTanks = oldEnemyTanks - 1 }) $ createSlowTank (location obj)
        writeIORef state $ updateObject newGame $ obj { duration = 0 }
        else
          writeIORef state $ updateObject game $ obj { duration = oldDuration + 100 }
  timerCallback state _ obj = return ()
