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
type TTimerCallback = IORef GameState -> KeyboardEvent -> Entity -> IO ()

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
    location :: TLocation,
    speed :: TSpeed,
    sprite :: String
  } |
  Tank {
    location :: TLocation,
    direction :: TLocation,
    diff :: TLocation,
    speed :: TSpeed,
    size :: TSize,
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

updateObject :: GameState -> ID -> Entity -> GameState
updateObject state id' obj = state { objects = (updateObject' $ objects state) }
  where
    updateObject' ((ci, x) : xs) =
      if ci == id' then (ci, obj) : xs else (ci, x) : updateObject' xs

registryObject :: GameState -> Entity -> GameState
registryObject state obj = state { objects = (genUniqueId state, obj) : (objects state) }

genUniqueId :: GameState -> ID
genUniqueId game = head $ dropWhile (\x -> isJust $ lookup x objs) [1 ..]
  where objs = objects game

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
getSprite tank@(Tank _ _ _ _ _ _ _ _ _ _) = sprite tank
getSprite bullet@(Bullet _ _ _) = sprite bullet

changeLocation :: TLocation -> TSpeed -> TLocation
changeLocation (x, y) (dx, dy) = (x+dx, y+dy)


isTank :: Entity -> Bool
isTank (Tank _ _ _ _ _ _ _ _ _ _) = True
isTank _ = False

isBullet :: Entity -> Bool
isBullet (Bullet _ _ _) = True
isBullet _ = False
