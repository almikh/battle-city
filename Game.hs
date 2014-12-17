module Game where

import Data.Ix
import Data.List
import Control.Monad
import Data.Array.IArray
import Data.Maybe
import Data.IORef
import KeyEvent
import Sprite

type TLocation = (Int, Int)
type TSpeed = (Int, Int)
type TSize = (Int, Int)
type TMessage = String

type TKeyboardCallback = IORef GameState -> KeyboardEvent -> Entity -> IO ()
type TCollisionCallback = IORef GameState -> (Entity, Entity) -> IO ()
type TTimerCallback = IORef GameState -> Int -> KeyboardEvent -> Entity -> IO ()

type TGrid = Array (Int, Int) Int -- нумерация - с 0

type ID = Int

data GameState =
  GameState {
    score :: Int,
    objects :: [(ID, Entity)],
    sprites :: [(String, Sprite)],
    pressedKey :: KeyboardEvent,
    grid :: TGrid
  }

data Entity =
  Bullet {
    eId :: ID,
    layer :: Int, -- для порядка отрисовки объектов
    location :: TLocation,
    speed :: TSpeed,
    diff :: TLocation,
    sprite :: String,
    onCollisionCallback :: TCollisionCallback,
    onKeyboardCallback :: TKeyboardCallback,
    onTimerCallback :: TTimerCallback
  } |
  Tank {
    eId :: ID,
    layer :: Int,
    location :: TLocation,
    direction :: TLocation,
    diff :: TLocation,
    speed :: TSpeed,
    size :: TSize,
    health :: Int,
    recharges :: Int,
    sprite :: String,
    rechargeTime :: Int,
    onCollisionCallback :: TCollisionCallback,
    onKeyboardCallback :: TKeyboardCallback,
    onTimerCallback :: TTimerCallback
  } |
  Obstacle {
    eId :: ID,
    layer :: Int,
    location :: TLocation,
    isImpassable :: Bool,
    isDestroyed :: Bool,
    health :: Int,
    sprite :: String,
    onCollisionCallback :: TCollisionCallback,
    onKeyboardCallback :: TKeyboardCallback,
    onTimerCallback :: TTimerCallback
  }

-- GameState
registrySprite :: GameState -> String -> Sprite -> GameState
registrySprite state name sprite = state { sprites = (name, sprite) : (sprites state) }

registrySprites :: GameState -> [(String, Sprite)] -> GameState
registrySprites state sprs = state { sprites = sprs ++ (sprites state) }

initGame :: GameState
initGame = GameState 0 [] [] No (newGrid 8 8)

updateObject :: GameState -> Entity -> GameState
updateObject state obj = state { objects = (updateObject' $ objects state) }
  where
    updateObject' ((ci, x) : xs) =
      if ci == (eId obj) then (ci, obj) : xs else (ci, x) : updateObject' xs

registryObject :: GameState -> Entity -> GameState
registryObject state obj = state { objects = (newId, obj { eId = newId }) : (objects state), grid = newGrid }
  where
    newId = genUniqueId state
    newGrid = (grid state) // [(location obj, newId)]

registryObjects :: GameState -> [Entity] -> GameState
registryObjects state objs = foldl registryObject state objs

deleteObject :: GameState -> Entity -> GameState
deleteObject state obj = state { objects = deleteObject' $ objects state }
  where
    deleteObject' ((i, o) : objs)
      | i == eId obj = objs
      | otherwise = (i, o) : deleteObject' objs

genUniqueId :: GameState -> ID
genUniqueId game = head $ dropWhile (\x -> isJust $ lookup x objs) [1 ..]
  where objs = objects game

getObjectFromCoord :: GameState -> (Int, Int) -> Entity
getObjectFromCoord state pos = obj
  where
    grid' = grid state
    Just obj = lookup (grid' ! pos) (objects state)

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
getSprite tank@(Tank _ _ _ _ _ _ _ _ _ _ _ _ _ _) = sprite tank
getSprite bullet@(Bullet _ _ _ _ _ _ _ _ _) = sprite bullet
getSprite obst@(Obstacle _ _ _ _ _ _ _ _ _ _) = sprite obst

changeLocation :: TLocation -> TSpeed -> TLocation
changeLocation (x, y) (dx, dy) = (x+dx, y+dy)

isTank :: Entity -> Bool
isTank (Tank _ _ _ _ _ _ _ _ _ _ _ _ _ _) = True
isTank _ = False

isBullet :: Entity -> Bool
isBullet (Bullet _ _ _ _ _ _ _ _ _) = True
isBullet _ = False

isObstacle :: Entity -> Bool
isObstacle (Obstacle _ _ _ _ _ _ _ _ _ _) = True
isObstacle _ = False

isImpassableObj :: Entity -> Bool
isImpassableObj (Obstacle _ _ _ is _ _ _ _ _ _) = is
isImpassableObj _ = True
