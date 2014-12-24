module Game where

import Data.Ix
import Data.List
import Control.Monad
import Data.Array.IArray
import Data.Maybe
import Data.IORef
import KeyEvent
import Sprite


screenRect :: ((Int, Int), (Int, Int))
screenRect = ((0, 0), (32*13, 32*13))

slowDt :: Int
slowDt = 36

normalDt :: Int
normalDt = 24

fastDt :: Int
fastDt = 12

type TLocation = (Int, Int)
type TSize = (Int, Int)
type TMessage = String

type TKeyboardCallback = IORef GameState -> KeyboardEvent -> ID -> IO ()
type TTimerCallback = IORef GameState -> Int -> ID -> IO ()

type TGrid = Array (Int, Int) Int -- нумерация - с 0

type TAnimation = [String]

type ID = Int

data GameState =
  GameState {
    fps :: Int,
    maxId :: Int,
    counter :: Int,
    enemyTanks :: Int,
    objects :: [(ID, Entity)],
    sprites :: [(String, Sprite)],
    keys :: [KeyboardEvent], -- нажатые в данный момент клавиши
    pause :: Bool,
    lifes :: Int
  }

data Entity =
  Bullet {
    eId :: ID,
    layer :: Int, -- для порядка отрисовки объектов
    location :: TLocation,
    direction :: TLocation,
    side :: Int, -- кто стрелял
    speed :: Int,
    size :: TSize,
    health :: Int,
    sprite :: TAnimation,
    onTimerCallback :: TTimerCallback
  } |
  Tank {
    eId :: ID,
    layer :: Int,
    location :: TLocation,
    direction :: TLocation,
    price :: Int,
    speed :: Int,
    size :: TSize,
    side :: Int,
    health :: Int,
    lifetime :: Int,
    duration :: Int,
    recharges :: Int,
    invulnerable :: Int,
    moveTowards :: Bool,
    sprite :: TAnimation,
    rechargeTime :: Int,
    targetDt :: Int,
    targetSprites :: TAnimation,
    spritesEffects :: TAnimation,
    onKeyboardCallback :: TKeyboardCallback,
    onTimerCallback :: TTimerCallback
  } |
  Obstacle {
    eId :: ID,
    layer :: Int,
    location :: TLocation,
    isImpassable :: Bool,
    isDestroyed :: Bool,
    isImmortal :: Bool,
    size :: TSize,
    health :: Int,
    sprite :: TAnimation,
    onTimerCallback :: TTimerCallback
  } |
  Boom {
    eId :: ID,
    layer :: Int,
    size :: TLocation,
    location :: TLocation,
    health :: Int,
    sprite :: TAnimation,
    onTimerCallback :: TTimerCallback
  } |
  Standart { -- знамя
    eId :: ID,
    layer :: Int,
    size :: TLocation,
    location :: TLocation,
    health :: Int,
    sprite :: TAnimation,
    onTimerCallback :: TTimerCallback
  } |
  RespawnPoint { -- отсюда будут появляться вражеские танки
    eId :: ID,
    layer :: Int,
    health :: Int,
    duration :: Int,
    size :: TLocation,
    location :: TLocation,
    onTimerCallback :: TTimerCallback
  }

-- GameState
registrySprite :: GameState -> String -> Sprite -> GameState
registrySprite state name sprite = state { sprites = (name, sprite) : (sprites state) }

registrySprites :: GameState -> [(String, Sprite)] -> GameState
registrySprites state sprs = state { sprites = sprs ++ (sprites state) }

initGame :: GameState
initGame = GameState 0 1 0 18 [] [] [] False 2

updateObject :: GameState -> Entity -> GameState
updateObject state obj = state { objects = (updateObject' $ objects state) }
  where
    updateObject' [] = []
    updateObject' ((ci, x) : xs) =
      if ci == (eId obj) then (ci, obj) : xs else (ci, x) : updateObject' xs

updateObjects :: GameState -> [Entity] -> GameState
updateObjects state objs = foldl updateObject state objs

registryObject :: GameState -> Entity -> GameState
registryObject state obj = state { maxId = newId+1, objects = (newId, obj { eId = newId }) : (objects state) }
  where
    newId = genUniqueId state

registryObjects :: GameState -> [Entity] -> GameState
registryObjects state objs = foldl registryObject state objs

deleteObject :: GameState -> Entity -> GameState
deleteObject state obj = state { objects = deleteObject' $ objects state }
  where
    deleteObject' [] = []
    deleteObject' ((i, o) : objs)
      | i == eId obj = objs
      | otherwise = (i, o) : deleteObject' objs

deleteObjectsIf :: GameState -> (Entity -> Bool) -> GameState
deleteObjectsIf state isDeleted = state { objects = newObjects }
  where
    newObjects = filter (\(_, o) -> not (isDeleted o)) $ objects state

-- чтобы не было возни с пересечение ID уничтоженного и вновь созданного объектов,
-- они будут уникальными для ВСЕХ объектов (размерности Int'а, думаю, для этой игры хватит)
genUniqueId :: GameState -> ID
genUniqueId game = maxId game
--genUniqueId game = head $ dropWhile (\x -> isJust $ lookup x objs) [1 ..]
--  where objs = objects game

-- Grid
newGrid :: Int -> Int -> TGrid
newGrid w h = listArray ((0, 0), (w-1, h-1)) $ replicate (w*h) 0

gridWidth :: TGrid -> Int
gridWidth grid = width
  where (_, (width, height)) = bounds grid

gridHeight :: TGrid -> Int
gridHeight grid = height
  where (_, (width, height)) = bounds grid

isValidInd :: TGrid -> Int -> Int -> Bool
isValidInd grid i j = l1 <= i && i <= width && l2 <= j && j <= height
  where ((l1, l2), (width, height)) = bounds grid

-- setValueOnGrid grid i j val =  grid // [((i, j), val)]
-- getId grid i j =               grid ! (i, j)

-- Entity
getSprite :: Entity -> String
getSprite obj = head $ sprite obj

getRect :: Entity -> TRect
getRect obj
  | isBoom obj = let
      (x, y) = location obj
      (w, h) = size obj in
      ((x - (w `div` 2), y - (h `div` 2)), (w, h))
  | otherwise = (location obj, size obj)

contains :: TRect -> TLocation -> Bool
contains ((x1, y1), (w1, h1)) (x, y) = containsX && containsY
  where
    containsX = x1 < x && x < x1+w1
    containsY = y1 < y && y < y1+h1

containsEq :: TRect -> TLocation -> Bool
containsEq ((x1, y1), (w1, h1)) (x, y) = containsX && containsY
  where
    containsX = x1 <= x && x <= x1+w1
    containsY = y1 <= y && y <= y1+h1

containsAny :: TRect -> [TLocation] -> Bool
containsAny rect xs = any (==True) $ map (contains rect) xs

isIntersect :: TRect -> TRect -> Bool
isIntersect r1@((x1, y1), (w1, h1)) r2@((x2, y2), (w2, h2)) = fContains || sContains || fPartContains || sPartContains
  where
    fPartContains = (x1==x2 && (x1+w1)==(x2+w2) && ((y2<y1 && y1 <(y2+h2)) || (y2<(y1+h1) && (y1+h1)<(y2+h2)))) ||
       (y1==y2 && (y1+h1)==(y2+h2) && ((x2<x1 && x1<(x2+w2)) || (x2<(x1+w1) && (x1+w1)<(x2+w2))))
    sPartContains = (x2==x1 && (x2+w2)==(x1+w1) && ((y1<y2 && y2<(y1+h1)) || (y1<(y2+h2) && (y2+h2)<(y1+h1)))) ||
       (y2==y1 && (y2+h2)==(y1+h1) && ((x1<x2 && x2<(x1+w1)) || (x1<(x2+w2) && (x2+w2)<(x1+w1))))
    fContains = containsAny r1 [(x2, y2), (x2+w2, y2), (x2+w2, y2+h2), (x2, y2+h2)]
    sContains = containsAny r2 [(x1, y1), (x1+w1, y1), (x1+w1, y1+h1), (x1, y1+h1)]

-- апервый прямоугольник содержит в себе второй
containsRect :: TRect -> TRect -> Bool
containsRect rect ((x, y), (w, h)) =
  all (==True) $ map (containsEq rect) $ [(x, y), (x+w, y), (x+w, y+h), (x, y+h)]

changeLocation :: TLocation -> (Int, Int) -> TLocation
changeLocation (x, y) (dx, dy) = (x+dx, y+dy)

isTank :: Entity -> Bool
isTank (Tank _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _) = True
isTank _ = False

isBullet :: Entity -> Bool
isBullet (Bullet _ _ _ _ _ _ _ _ _ _) = True
isBullet _ = False

isObstacle :: Entity -> Bool
isObstacle (Obstacle _ _ _ _ _ _ _ _ _ _) = True
isObstacle _ = False

isBoom :: Entity -> Bool
isBoom (Boom _ _ _ _ _ _ _) = True
isBoom _ = False

isStandart :: Entity -> Bool
isStandart (Standart _ _ _ _ _ _ _) = True
isStandart _ = False

isRespawnPoint :: Entity -> Bool
isRespawnPoint (RespawnPoint _ _ _ _ _ _ _) = True
isRespawnPoint _ = False

isImpassableObj :: Entity -> Bool
isImpassableObj (Obstacle _ _ _ is _ _ _ _ _ _) = is
isImpassableObj _ = True
