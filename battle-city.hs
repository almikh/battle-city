import Graphics.UI.GLUT hiding (renderObject, Objects)
import Control.Applicative
import Control.Monad
import Data.Array.IArray
import Data.IORef
import Data.List
import Game
import Creators
import KeyEvent
import Sprite

screenWidth = 48*8
screenHeight = 48*8

main :: IO ()
main = do
  (this, args) <- getArgsAndInitialize

  state <- newIORef $ initGame
  initMap state
  initSprites state
  initObjects state

  window <- createWindow "Battle City"
  windowSize $= Size (48*8) (48*8)
  initialDisplayMode $= [ RGBAMode, DoubleBuffered, WithDepthBuffer ]

  blend $= Enabled
  blendFunc $= (SrcAlpha, OneMinusSrcAlpha)

  idleCallback $= Just (idle state)
  displayCallback $= (display state)
  reshapeCallback $= Just resize
  keyboardCallback  $= Just (keyboard state)
  keyboardUpCallback  $= Just (keyboardUp state)

  addTimerCallback 25 (timerDt state 25)
  addTimerCallback 15 (bulletsTimer state 15)

  mainLoop

initMap :: IORef GameState -> IO ()
initMap state = do
  putStrLn "Map loaded..."

initSprites :: IORef GameState -> IO ()
initSprites state = do
  game <- readIORef state

  sprite <- loadSprite "test.pic"
  spriteArmor <- loadSprite "resources/armor.pic"
  spriteGrass <- loadSprite "resources/grass.pic"
  spriteWater <- loadSprite "resources/water.pic"
  spriteBrick <- loadSprite "resources/brick.pic"
  spriteBullet <- loadSprite "resources/bullet.pic"

  let sprites = [
        ("test", sprite),
        ("armor", spriteArmor),
        ("grass", spriteGrass),
        ("water", spriteWater),
        ("brick", spriteBrick),
        ("bullet", spriteBullet) ]
  writeIORef state $ registrySprites game sprites
  putStrLn "Sprites loaded..."

initObjects :: IORef GameState -> IO ()
initObjects state = do
  game <- readIORef state
  let objs = [
        createHero (2, 2),
        createGrass (3, 3),
        createGrass (4, 4),
        createArmor (5, 5),
        createBrick (5, 6),
        createWater (7, 7)
        ]
  writeIORef state $ registryObjects game objs
  putStrLn "Objects loaded..."

renderObject :: GameState -> Entity -> IO ()
renderObject game obj= do
  let Just sprite = lookup (getSprite obj) (sprites game)
  let (x, y) = location obj
  let zoom = (cellSize `div` 16)
  if isTank obj then do
    let (dx, dy) = diff obj
    let dir = dir2rot $ direction obj
    drawSpriteEx dir (x*cellSize + dx) (y*cellSize + dy) sprite zoom
    else if isBullet obj then do
      let (dx, dy) = diff obj
      drawSprite (x*cellSize + dx) (y*cellSize + dy) sprite zoom
      else
        drawSprite (x*cellSize) (y*cellSize) sprite zoom


resize :: Size -> IO ()
resize s@(Size w h) = do
  viewport $= (Position 0 0, s)
  matrixMode $= Projection
  loadIdentity
  ortho2D 0 (realToFrac w) 0 (realToFrac h)

display :: IORef GameState -> IO ()
display state = do
  clear [ ColorBuffer, DepthBuffer ]

  let cellsInRow = screenWidth `div` cellSize
  let cellsInCol = screenHeight `div` cellSize
  let rows = take cellsInRow $ map (\i -> ((0, i*cellSize), (screenWidth, i*cellSize))) [1 .. ]
  let cols = take cellsInCol $ map (\i -> ((i*cellSize, 0), (i*cellSize, screenHeight))) [1 .. ]

  game <- readIORef state
  let grid' = grid game
  let width = gridWidth $ grid'
  let height = gridHeight $ grid'
  currentColor $= Color4 1.0 1.0 1.0 1.0
  forM_ [0 .. width] $ \i ->
    forM_ [0 .. height] $ \j -> do
      -- setPolygonMode' grid' i j
      renderPrimitive Quads $ do
        vertex $ Vertex3 (i2f (i*cellSize)) (i2f (j*cellSize)) 0
        vertex $ Vertex3 (i2f (i*cellSize)) (i2f ((j+1)*cellSize)) 0
        vertex $ Vertex3 (i2f ((i+1)*cellSize)) (i2f ((j+1)*cellSize)) 0
        vertex $ Vertex3 (i2f ((i+1)*cellSize)) (i2f (j*cellSize)) 0

  polygonMode $= (Line, Line)
  let sortFunc = \o1 o2 -> layer (snd o1) `compare` layer (snd o2)
  mapM_ (\(_, o) -> renderObject game o) $ sortBy sortFunc $ objects game
  flush

setPolygonMode' :: TGrid -> Int -> Int -> IO ()
setPolygonMode' g i j =
  if g ! (i, j) == 0 then polygonMode $= (Line, Line)
    else polygonMode $= (Fill, Fill)

timerDt :: IORef GameState -> Int -> IO ()
timerDt state elapsed = do
  game <- readIORef state
  let pressed = pressedKey game
  mapM_ (\(i, o) -> (onTimerCallback o) state elapsed pressed o) $ objects game
  addTimerCallback elapsed (timerDt state elapsed)
  return ()

bulletsTimer :: IORef GameState -> Int -> IO ()
bulletsTimer state elapsed = do
  game <- readIORef state
  let pressed = pressedKey game
  mapM_ (\(i, o) -> (onTimerCallback o) state elapsed pressed o) $ filter (isBullet . snd) $ objects game
  addTimerCallback elapsed (bulletsTimer state elapsed)
  return ()

idle :: IORef GameState -> IO ()
idle state = do
  game <- readIORef state
  postRedisplay Nothing

keyboard :: IORef GameState -> Char -> Position -> IO ()
keyboard state ch _ = do
  game <- readIORef state
  let key = event ch
  -- mapM_ (\(i, o) -> (onKeyboardCallback o) state key o) $ objects game
  game <- readIORef state
  writeIORef state $ game { pressedKey = key }


keyboardUp :: IORef GameState -> Char -> Position -> IO ()
keyboardUp state ch _ = do
  game <- readIORef state
  writeIORef state $ game { pressedKey = No }
  return ()
