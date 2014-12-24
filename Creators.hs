{-# LANGUAGE TupleSections #-}
module Creators where

import Game
import System.Random
import Control.Monad
import Data.Array.IArray
import qualified Data.Map as Map
import Data.IORef
import Data.List
import Data.Maybe
import KeyEvent
import PathFinder

steps = 12
cellSize = 32
screenWidth :: Int
screenWidth = 32*13

screenHeight :: Int
screenHeight = 32*13

respawnTime :: Int
respawnTime = 20000

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

createAccessGrid :: GameState -> TGrid
createAccessGrid game = foldl foldFunc (newGrid 13 13) $ map snd obstacles
  where
    obstacles = filter (\(i, o) -> isCollidingObject o && notEffect o) $ objects game
    foldFunc grid o = let (x, y) = location o in grid // [((x `div` cellSize, y `div` cellSize), -1)]

printGrid :: TGrid -> IO ()
printGrid grid = do
  putStrLn "==="
  let h = gridHeight grid
  forM_ [0 .. (gridHeight grid)] $ \j -> do
    forM_ [0 .. (gridWidth grid)] $ \i -> do
      if grid ! (i, h-j) < 0 then
        putStr $ " " ++ show (grid ! (i, h-j))
        else
          putStr $ "  " ++ show (grid ! (i, h-j))
    putStrLn ""
  return ()

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
    moveTowards = False,
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
        let newDuration = if duration obj < respawnTime then duration obj + dt else 0
        let newRecharge = (\r -> if r /= 0 then r - dt else r) $ recharges obj

        let changeDirection = \o -> do
            num <- randomIO :: IO Int
            part <- randomIO :: IO Int
            let periodDuration = respawnTime `div` 8
                (x, y) = location o
            if newDuration < periodDuration then do -- двигаться случайно
              let sdir = if num `mod` 2 == 0 then -1 else 1
                  newDir@(xdir, ydir) = if part `mod` 2 == 0 then (sdir, 0) else (0, sdir)
              putStrLn "random move..."
              writeIORef state $ updateObject game $ o { moveTowards = False, duration = newDuration, direction = newDir, recharges = newRecharge }
              else if newDuration < periodDuration*3 then do -- двигаться к игроку
                let tempGrid = createAccessGrid game
                    targets = filter (\(i, o) -> isTank o && side o == 1) $ objects game
                when (not $ null targets) $ do
                  let target = snd $ head targets
                      (tx, ty) = location target
                      thisCell@(cx, cy) = (x `div` cellSize, y `div` cellSize)
                      targetCell = (tx `div` cellSize, ty `div` cellSize)
                      accessGrid = tempGrid // [(targetCell, 0), (thisCell, 0)]
                      path = findPath accessGrid thisCell targetCell
                  when (length path > 1) $ do
                    let nextCell@(nx, ny) = head $ tail path
                        newDir@(xdir, ydir) = (nx - cx, ny - cy)
                    putStrLn "find hero..."
                    writeIORef state $ updateObject game $ o { moveTowards = True, duration = newDuration, direction = newDir, recharges = newRecharge }
                else do -- двигаться к штабу
                  let eagle = snd $ head $ filter (isStandart . snd) others
                      tempGrid = createAccessGrid game
                      thisCell@(tx, ty) = (x `div` cellSize, y `div` cellSize)
                      fGrid = runWave tempGrid thisCell
                      eagleLocation@(ex, ey) = location eagle
                      eagleCell@(ecx, ecy) = (ex `div` cellSize, ey `div` cellSize)
                      sGrid = runWave (newGrid 13 13) eagleCell
                      foldFunc d (i, e) = if e > 0 then Map.insert key newVal d else d
                        where
                          key = sGrid ! i
                          oldVal = if isJust (Map.lookup key d) then fromJust (Map.lookup key d) else []
                          newVal = (i, fGrid ! i) : oldVal
                      dictionary = foldl foldFunc Map.empty (assocs fGrid)

                  let ls = Map.assocs dictionary
                      selectTargetCell ((w, cells) : []) = fst $ minimumBy (\x y -> snd x `compare` snd y) cells
                      selectTargetCell ((w, cells) : oths) = if (fromIntegral price1) > (fromIntegral price2)*1.5 then cell2 else cell1
                        where
                          min1@(cell1, price1) = minimumBy (\x y -> snd x `compare` snd y) cells
                          min2@(cell2, price2) = minimumBy (\x y -> snd x `compare` snd y) $ snd $ head oths

                  let targetCell = selectTargetCell ls
                      accessGrid = tempGrid // [(thisCell, 0)]
                      path = findPath accessGrid thisCell targetCell

                  if (length path > 1) then do
                    let nextCell@(nx, ny) = head $ tail path
                        newDir@(xdir, ydir) = (nx - tx, ny - ty)
                    putStrLn "find eagle..."
                    writeIORef state $ updateObject game $ o { moveTowards = True, duration = newDuration, direction = newDir, recharges = newRecharge }
                    else do
                      let sdir = if num `mod` 2 == 0 then -1 else 1
                          newDir@(xdir, ydir) = if part `mod` 2 == 0 then (sdir, 0) else (0, sdir)
                      putStrLn "forced random move..."
                      writeIORef state $ updateObject game $ o { moveTowards = False, duration = newDuration, direction = newDir, recharges = newRecharge }
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
            let periodDuration = 1 + respawnTime `div` 8
                alignInCell = (x `mod` cellSize == 0) && (y `mod` cellSize == 0)
                freq = max 1 (8 - (duration o) `div` periodDuration)
            -- чем дальше - тем танк начинает больше суетиться
            if alignInCell && (moveTowards o || num `mod` freq == 0) then do
              changeDirection o
              else do
                num <- randomIO :: IO Int
                let dir@(xdir, ydir) = direction o
                    newLocation@(nx, ny) = changeLocation coord (xdir*spd, ydir*spd)
                    newObj = o { location = newLocation }
                    collision = filter (\(_, e) -> isCollidingObject e && (getRect newObj) `isIntersect` (getRect e)) $ others
                    flag = containsRect screenRect (getRect newObj) && null collision
                if not flag && (num `mod` 16 == 0) then do
                  if alignInCell then do -- invert direction
                    writeIORef state $ updateObject game $ o { moveTowards = False, duration = newDuration, direction = (-xdir, -ydir), recharges = newRecharge }
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
            writeIORef state $ updateObject game $ obj { moveTowards = False, duration = newDuration, recharges = newRecharge }

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

      -- let path = findPath (createAccessGrid game) (4, 1) (0, 12)
      -- printGrid $ (createAccessGrid game) // (map (, -3) path)

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
          writeIORef state $ updateObject game $ obj { lifetime = newLife, sprite = ts, invulnerable = 12, spritesEffects = se }
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
                          newGame = deleteObjectsIf (updateObjects game updated) (\o -> eId o == id' || health o <= 0)
                      writeIORef state $ updateObject (newGame { lifes = newLifes }) $ eagle { health = 100500, sprite = ["fall_standart"] }
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
        -- let newGame = registryObject (game { enemyTanks = oldEnemyTanks - 1 }) $ createSlowTank (location obj)
        let newGame = game { enemyTanks = oldEnemyTanks - 1 }
        writeIORef state $ updateObject newGame $ obj { duration = 0 }
        else
          writeIORef state $ updateObject game $ obj { duration = oldDuration + 100 }
  timerCallback state _ obj = return ()
