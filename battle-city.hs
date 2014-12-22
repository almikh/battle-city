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

import Control.Monad(when)
import System.Exit(exitFailure)
import System.IO(withBinaryFile, IOMode(ReadMode), openBinaryFile, hGetBuf)
import Foreign.Marshal.Alloc(allocaBytes)

borderSize = 32

main :: IO ()
main = do
  (this, args) <- getArgsAndInitialize

  state <- newIORef $ initGame

  window <- createWindow "Battle City"
  windowSize $= Size (32*13 + 64 + 32*2) (32*13 + 32*2)
  initialDisplayMode $= [ RGBAMode, DoubleBuffered, WithDepthBuffer ]

  blend $= Enabled
  blendFunc $= (SrcAlpha, OneMinusSrcAlpha)

  initMap state
  initSprites state
  initObjects state

  clearColor $= Color4 0.45 0.45 0.45 1.0
  idleCallback $= Just (idle state)
  displayCallback $= (display state)
  reshapeCallback $= Just resize
  keyboardCallback  $= Just (keyboard state)
  keyboardUpCallback  $= Just (keyboardUp state)

  addTimerCallback slowDt (timerDt state slowDt)
  addTimerCallback averageDt (timerDt state averageDt)
  addTimerCallback fastDt (timerDt state fastDt)
  addTimerCallback 12 (bulletsTimer state 12)
  addTimerCallback 100 (animTimer state 100)
  addTimerCallback 1000 (fpsTimer state 1000)

  mainLoop

initMap :: IORef GameState -> IO ()
initMap state = do
  grid <- loadMap "grid.map"
  let h = gridHeight grid
      ds = cellSize `div` 2
  forM_ [0 .. gridWidth grid] $ \i ->
    forM_ [0 .. gridHeight grid] $ \j -> do
      game <- readIORef state
      case grid ! (h-j, i) of
        1 -> do
          let obj = createBrick (i*ds, j*ds)
          writeIORef state $ registryObject game obj
        2 -> do
          let obj = createArmor (i*ds, j*ds)
          writeIORef state $ registryObject game obj
        3 -> do
          let obj = createGrass (i*ds, j*ds)
          writeIORef state $ registryObject game obj
        4 -> do
          let obj = createStandart (i*ds, j*ds)
          writeIORef state $ registryObject game obj
        5 -> do
          let obj = createRespawnPoint (i*ds, j*ds)
          writeIORef state $ registryObject game obj
        _ -> return ()
  putStrLn "Map loaded..."

initSprites :: IORef GameState -> IO ()
initSprites state = do
  game <- readIORef state

  spriteArmor <- loadSprite "resources/armor_8x8.pic"
  spriteGrass <- loadSprite "resources/grass_8x8.pic"
  spriteWater <- loadSprite "resources/water_8x8.pic"
  spriteBrick <- loadSprite "resources/brick_8x8.pic"
  spriteBullet <- loadSprite "resources/bullet_16x16.pic"
  spriteStar0 <- loadSprite "resources/star0_16x16.pic"
  spriteStar1 <- loadSprite "resources/star1_16x16.pic"
  spriteStar2 <- loadSprite "resources/star2_16x16.pic"
  spriteStar3 <- loadSprite "resources/star3_16x16.pic"
  boom0 <- loadSprite "resources/boom0_16x16.pic"
  boom1 <- loadSprite "resources/boom1_16x16.pic"
  boom2 <- loadSprite "resources/boom2_16x16.pic"
  bigboom0 <- loadSprite "resources/bigboom0_32x32.pic"
  bigboom1 <- loadSprite "resources/bigboom1_32x32.pic"
  fallenStandart <- loadSprite "resources/fallenstandart_16x16.pic"
  standart <- loadSprite "resources/standart_16x16.pic"
  gameover <- loadSprite "resources/gameover_301x199.pic"
  hero0 <- loadSprite "resources/goldtank00_15x15.pic"
  hero1 <- loadSprite "resources/goldtank01_15x15.pic"
  average0 <- loadSprite "resources/avtank00_15x15.pic"
  average1 <- loadSprite "resources/avtank01_15x15.pic"
  slow0 <- loadSprite "resources/slowtank00_15x15.pic"
  slow1 <- loadSprite "resources/slowtank01_15x15.pic"
  fast0 <- loadSprite "resources/fasttank00_15x15.pic"
  fast1 <- loadSprite "resources/fasttank01_15x15.pic"

  let sprites = [
        ("hero0", hero0),
        ("hero1", hero1),
        ("average0", average0),
        ("average1", average1),
        ("slow0", slow0),
        ("slow1", slow1),
        ("fast0", fast0),
        ("fast1", fast1),
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
        ("fall_standart", fallenStandart),
        ("gameover", gameover) ]
  writeIORef state $ registrySprites game sprites
  putStrLn "Sprites loaded..."

initObjects :: IORef GameState -> IO ()
initObjects state = do
  game <- readIORef state
  let objs = [
        createHero (4*cellSize, 0*cellSize),
        createSlowTank (0*cellSize, 12*cellSize),
        createFastTank (12*cellSize, 12*cellSize),
        createAvTank (6*cellSize, 12*cellSize) ]
  writeIORef state $ registryObjects game objs
  putStrLn "Objects loaded..."

renderObject :: GameState -> Entity -> IO ()
renderObject game obj
  | isTank obj = do
    let Just sprite = lookup (getSprite obj) (sprites game)
        dir = dir2rot $ direction obj
    drawSpriteEx dir sprite rect
  | isRespawnPoint obj = do
    polygonMode $= (Line, Line)
    currentColor $= Color4 1.0 1.0 1.0 1.0
    renderPrimitive Quads $ do
      vertex $ Vertex2 (i2f (x+borderSize)) (i2f (y+borderSize))
      vertex $ Vertex2 (i2f (x+borderSize)) (i2f (y+h+borderSize))
      vertex $ Vertex2 (i2f (x+w+borderSize)) (i2f (y+h+borderSize))
      vertex $ Vertex2 (i2f (x+w+borderSize)) (i2f (y+borderSize))
    polygonMode $= (Fill, Fill)
  | otherwise = do
    let Just sprite = lookup (getSprite obj) (sprites game)
    drawSprite sprite rect
    where
      objRect@((x, y), (w, h)) = getRect obj
      rect = ((x+borderSize, y+borderSize), (w, h))

resize :: Size -> IO ()
resize s@(Size w h) = do
  viewport $= (Position 0 0, s)
  matrixMode $= Projection
  loadIdentity
  ortho2D 0 (realToFrac w) 0 (realToFrac h)

display :: IORef GameState -> IO ()
display state = do
  clear [ ColorBuffer, DepthBuffer ]

  currentColor $= Color4 0.0 0.0 0.0 1.0
  renderPrimitive Quads $ do
    vertex $ Vertex3 (i2f borderSize) (i2f borderSize) 0
    vertex $ Vertex3 (i2f borderSize) (i2f (borderSize+screenHeight)) 0
    vertex $ Vertex3 (i2f (borderSize+screenWidth)) (i2f (borderSize+screenHeight)) 0
    vertex $ Vertex3 (i2f (borderSize+screenWidth)) (i2f borderSize) 0

  currentColor $= Color4 1.0 1.0 1.0 1.0
  game <- readIORef state
  writeIORef state $ game { counter = counter game + 1 }
  game <- readIORef state

  -- polygonMode $= (Fill, Fill)
  let sortFunc = \o1 o2 -> layer (snd o1) `compare` layer (snd o2)
  mapM_ (\(_, o) -> renderObject game o) $ sortBy sortFunc $ objects game

  let standart = snd $ head $ filter (isStandart . snd) $ objects game
  when (health standart > 1) $ do
    currentColor $= Color4 0.0 0.0 0.0 0.7
    renderPrimitive Quads $ do
      vertex $ Vertex3 (i2f borderSize) (i2f borderSize) 0
      vertex $ Vertex3 (i2f borderSize) (i2f (borderSize+screenHeight)) 0
      vertex $ Vertex3 (i2f (borderSize+screenWidth)) (i2f (borderSize+screenHeight)) 0
      vertex $ Vertex3 (i2f (borderSize+screenWidth)) (i2f borderSize) 0
    let Just sprite@(Sprite w h _) = lookup "gameover" $ sprites game
    let rect = (((screenWidth-w) `div` 2, (screenHeight-h) `div` 2), (w, h))
    currentColor $= Color4 1.0 1.0 1.0 1.0
    drawSprite sprite rect
  --preservingMatrix $ do
  --  translate $ Vector3 (i2f 100) 0.0 0.0
  --  renderString Fixed8By13 . ("FPS: " ++) $ show (fps game)
  -- putStrLn $ ("FPS: " ++) $ show (fps game)
  flush

animTimer :: IORef GameState -> Int -> IO ()
animTimer state elapsed = do
  game <- readIORef state

  mapM_ (\(i, o) -> (onTimerCallback o) state elapsed i) $ objects game
  addTimerCallback elapsed (animTimer state elapsed)
  return ()

timerDt :: IORef GameState -> Int -> IO ()
timerDt state elapsed = do
  game <- readIORef state
  mapM_ (\(i, o) -> (onTimerCallback o) state elapsed i) $ objects game
  addTimerCallback elapsed (timerDt state elapsed)
  return ()

bulletsTimer :: IORef GameState -> Int -> IO ()
bulletsTimer state elapsed = do
  game <- readIORef state
  mapM_ (\(i, o) -> (onTimerCallback o) state elapsed i) $ filter (isBullet . snd) $ objects game
  addTimerCallback elapsed (bulletsTimer state elapsed)
  return ()

fpsTimer :: IORef GameState -> Int -> IO ()
fpsTimer state elapsed = do
  game <- readIORef state
  writeIORef state $ game { counter = 0, fps = counter game }
  addTimerCallback elapsed (fpsTimer state elapsed)
  return ()

idle :: IORef GameState -> IO ()
idle state = do
  postRedisplay Nothing
  return ()

keyboard :: IORef GameState -> Char -> Position -> IO ()
keyboard state ch _ = do
  game <- readIORef state

  let standart = snd $ head $ filter (isStandart . snd) $ objects game
  when (health standart == 1) $ do
    let key = event ch
    if key /= Fire then do
      let oldKeys = delete MoveLeftward $ delete MoveRightward $ delete MoveForward $ delete MoveBackward $ (keys game)
      writeIORef state $ game { keys = oldKeys ++ [key] }
      else writeIORef state $ game { keys = (delete key (keys game)) ++ [key] }

keyboardUp :: IORef GameState -> Char -> Position -> IO ()
keyboardUp state ch _ = do
  game <- readIORef state
  let key = event ch
  writeIORef state $ game { keys = delete key (keys game) }
  return ()
