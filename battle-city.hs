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
  windowSize $= Size (48*13) (48*13)
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
  addTimerCallback 100 (animTimer state 100)
  addTimerCallback 1000 (fpsTimer state 1000)

  mainLoop

initMap :: IORef GameState -> IO ()
initMap state = do
  grid <- loadMap "grid.map"
  let h = gridHeight grid
  forM_ [0 .. gridWidth grid] $ \i ->
    forM_ [0 .. 4{-gridHeight grid-}] $ \j -> do
      case grid ! (h-j, i) of
        1 -> do
          game <- readIORef state
          let objs = [
                createBrick (i*cellSize, j*cellSize),
                createBrick (i*cellSize+24, j*cellSize),
                createBrick (i*cellSize, j*cellSize+24),
                createBrick (i*cellSize+24, j*cellSize+24)]
          writeIORef state $ registryObjects game objs
        2 -> do
          game <- readIORef state
          let objs = [
                createArmor (i*cellSize, j*cellSize),
                createArmor (i*cellSize+24, j*cellSize),
                createArmor (i*cellSize, j*cellSize+24),
                createArmor (i*cellSize+24, j*cellSize+24)]
          writeIORef state $ registryObjects game objs
        3 -> do
          game <- readIORef state
          let objs = [
                createGrass (i*cellSize, j*cellSize),
                createGrass (i*cellSize+24, j*cellSize),
                createGrass (i*cellSize, j*cellSize+24),
                createGrass (i*cellSize+24, j*cellSize+24)]
          writeIORef state $ registryObjects game objs
        4 -> do
          game <- readIORef state
          let obj = createStandart (i*cellSize+24, j*cellSize+24)
          writeIORef state $ registryObject game obj
        otherwise -> return ()
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
  spriteStar0 <- loadSprite "resources/star0.pic"
  spriteStar1 <- loadSprite "resources/star1.pic"
  spriteStar2 <- loadSprite "resources/star2.pic"
  spriteStar3 <- loadSprite "resources/star3.pic"
  boom0 <- loadSprite "resources/boom0.pic"
  boom1 <- loadSprite "resources/boom1.pic"
  boom2 <- loadSprite "resources/boom2.pic"
  bigboom0 <- loadSprite "resources/bigboom0.pic"
  bigboom1 <- loadSprite "resources/bigboom1.pic"
  fallenStandart <- loadSprite "resources/fallen_standart.pic"
  standart <- loadSprite "resources/standart.pic"

  let sprites = [
        ("test", sprite),
        ("armor", spriteArmor),
        ("grass", spriteGrass),
        ("water", spriteWater),
        ("brick", spriteBrick),
        ("bullet", spriteBullet),
        ("star:0", spriteStar0),
        ("star:1", spriteStar1),
        ("star:2", spriteStar2),
        ("star:3", spriteStar3),
        ("boom:0", boom0),
        ("boom:1", boom1),
        ("boom:2", boom2),
        ("bigboom:0", bigboom0),
        ("bigboom:1", bigboom1),
        ("standart", standart),
        ("fall_standart", fallenStandart) ]
  writeIORef state $ registrySprites game sprites
  putStrLn "Sprites loaded..."

initObjects :: IORef GameState -> IO ()
initObjects state = do
  game <- readIORef state
  let objs = [
        createHero (4*cellSize, 0*cellSize),
        createSlowTank (0*cellSize, 12*cellSize) ]
  writeIORef state $ registryObjects game objs
  putStrLn "Objects loaded..."

renderObject :: GameState -> Entity -> IO ()
renderObject game obj
  | isTank obj = do
    let dir = dir2rot $ direction obj
    drawSpriteEx dir sprite rect
    {-
    currentColor $= Color4 1.0 1.0 1.0 1.0
    renderPrimitive Quads $ do
      vertex $ Vertex3 (i2f x) (i2f y) 0
      vertex $ Vertex3 (i2f x) (i2f (y+h)) 0
      vertex $ Vertex3 (i2f (x+w)) (i2f (y+h)) 0
      vertex $ Vertex3 (i2f (x+w)) (i2f y) 0
    -}
  | otherwise = drawSprite sprite rect
    where
      Just sprite = lookup (getSprite obj) (sprites game)
      rect@((x, y), (w, h)) = getRect obj

resize :: Size -> IO ()
resize s@(Size w h) = do
  viewport $= (Position 0 0, s)
  matrixMode $= Projection
  loadIdentity
  ortho2D 0 (realToFrac w) 0 (realToFrac h)

display :: IORef GameState -> IO ()
display state = do
  clear [ ColorBuffer, DepthBuffer ]

  game <- readIORef state
  writeIORef state $ game { counter = counter game + 1 }
  game <- readIORef state

  polygonMode $= (Line, Line)
  let sortFunc = \o1 o2 -> layer (snd o1) `compare` layer (snd o2)
  mapM_ (\(_, o) -> renderObject game o) $ sortBy sortFunc $ objects game

  --preservingMatrix $ do
  --  translate $ Vector3 (i2f 100) 0.0 0.0
  --  renderString Fixed8By13 . ("FPS: " ++) $ show (fps game)
  -- putStrLn $ ("FPS: " ++) $ show (fps game)
  flush

setPolygonMode' :: TGrid -> Int -> Int -> IO ()
setPolygonMode' g i j =
  if g ! (i, j) == 0 then polygonMode $= (Line, Line)
    else polygonMode $= (Fill, Fill)

animTimer :: IORef GameState -> Int -> IO ()
animTimer state elapsed = do
  game <- readIORef state
  let pressed = pressedKey game
  mapM_ (\(i, o) -> (onTimerCallback o) state elapsed pressed i) $ objects game
  addTimerCallback elapsed (animTimer state elapsed)
  return ()

timerDt :: IORef GameState -> Int -> IO ()
timerDt state elapsed = do
  game <- readIORef state
  let pressed = pressedKey game
  mapM_ (\(i, o) -> (onTimerCallback o) state elapsed pressed i) $ objects game
  addTimerCallback elapsed (timerDt state elapsed)
  return ()

bulletsTimer :: IORef GameState -> Int -> IO ()
bulletsTimer state elapsed = do
  --game <- readIORef state
  --let pressed = pressedKey game
  --mapM_ (\(i, o) -> (onTimerCallback o) state elapsed pressed o) $ filter (isBullet . snd) $ objects game
  --addTimerCallback elapsed (bulletsTimer state elapsed)
  return ()

fpsTimer :: IORef GameState -> Int -> IO ()
fpsTimer state elapsed = do
  game <- readIORef state
  writeIORef state $ game { counter = 0, fps = counter game }
  addTimerCallback elapsed (fpsTimer state elapsed)
  return ()

idle :: IORef GameState -> IO ()
idle state = do
  game <- readIORef state
  postRedisplay Nothing

keyboard :: IORef GameState -> Char -> Position -> IO ()
keyboard state ch _ = do
  game <- readIORef state
  let key = event ch
  mapM_ (\(i, o) -> (onKeyboardCallback o) state key i) $ objects game
  game <- readIORef state
  writeIORef state $ game { pressedKey = key }


keyboardUp :: IORef GameState -> Char -> Position -> IO ()
keyboardUp state ch _ = do
  game <- readIORef state
  writeIORef state $ game { pressedKey = No }
  return ()
