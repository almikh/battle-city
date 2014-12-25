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
import PathFinder

import Control.Monad(when)
import System.Exit(exitFailure)
import System.IO(withBinaryFile, IOMode(ReadMode), openBinaryFile, hGetBuf)
import Foreign.Marshal.Alloc(allocaBytes)

borderSize = 32

test :: IO ()
test = do
  let grid = newGrid 5 5
      new = runWave (grid // [((0, 1), -1), ((1, 0), -1)]) (1, 3)
      path = findPath new (1, 3) (4, 0)
  mapM_ print path
  forM_ [0 .. (gridWidth grid)] $ \i -> do
    forM_ [0 .. (gridHeight grid)] $ \j -> do
      if new ! (i, j) < 0 then
        putStr $ " " ++ show (new ! (i, j)) ++ " "
        else
          putStr $ "  " ++ show (new ! (i, j)) ++ " "
    putStrLn ""
  return ()

main :: IO ()
main = do
  (this, args) <- getArgsAndInitialize

  -- test

  state <- newIORef $ initGame

  window <- createWindow "Battle City"
  windowSize $= Size (32*13 + 64 + 32) (32*13 + 32*2)
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
  addTimerCallback normalDt (timerDt state normalDt)
  addTimerCallback fastDt (timerDt state fastDt)
  addTimerCallback 600 (timerDt state 600)
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
          let obj = createWater (i*ds, j*ds)
          writeIORef state $ registryObject game obj
        5 -> do
          let obj = createStandart (i*ds, j*ds)
          writeIORef state $ registryObject game obj
        6 -> do
          let obj = createRespawnPoint (i*ds, j*ds)
          writeIORef state $ registryObject game obj
        _ -> return ()
  putStrLn "Map loaded..."

initSprites :: IORef GameState -> IO ()
initSprites state = do
  let sprites = [
        ("field:0", "resources/field0_16x16.pic"),
        ("field:1", "resources/field1_16x16.pic"),
        ("pause", "resources/pause_39x8.pic"),
        ("icon", "resources/tankicon_16x16.pic"),
        ("bullet", "resources/bullet_8x8.pic"),
        ("armor", "resources/armor_8x8.pic"),
        ("grass", "resources/grass_8x8.pic"),
        ("water:0", "resources/water0_8x8.pic"),
        ("water:1", "resources/water1_8x8.pic"),
        ("water:2", "resources/water2_8x8.pic"),
        ("brick", "resources/brick_8x8.pic"),
        ("star:0", "resources/star0_16x16.pic"),
        ("star:1", "resources/star1_16x16.pic"),
        ("star:2", "resources/star2_16x16.pic"),
        ("star:3", "resources/star3_16x16.pic"),
        ("boom:0", "resources/boom0_16x16.pic"),
        ("boom:1", "resources/boom1_16x16.pic"),
        ("boom:2", "resources/boom2_16x16.pic"),
        ("bigboom:0", "resources/bigboom0_32x32.pic"),
        ("bigboom:1", "resources/bigboom1_32x32.pic"),
        ("fall_standart", "resources/fallenstandart_16x16.pic"),
        ("standart", "resources/standart_16x16.pic"),
        ("hero0", "resources/goldtank00_15x15.pic"),
        ("hero1", "resources/goldtank01_15x15.pic"),
        ("gameover", "resources/gameover_301x199.pic"),
        ("normal0", "resources/normaltank00_15x15.pic"),
        ("normal1", "resources/normaltank01_15x15.pic"),
        ("heavy0", "resources/heavytank00_15x15.pic"),
        ("heavy1", "resources/heavytank01_15x15.pic"),
        ("rapid0", "resources/rapidtank00_15x15.pic"),
        ("rapid1", "resources/rapidtank01_15x15.pic"),
        ("fast0", "resources/fasttank00_15x15.pic"),
        ("fast1", "resources/fasttank01_15x15.pic"),
        ("numbers", "resources/numbers_160x16.pic"),
        ("life", "resources/life_40x40.pic"),
        ("100", "resources/100_15x15.pic"),
        ("200", "resources/200_15x15.pic"),
        ("300", "resources/300_15x15.pic"),
        ("400", "resources/400_15x15.pic"),
        ("500", "resources/500_15x15.pic"),
        ("clock", "resources/clock_15x15.pic"),
        ("helmet", "resources/helmet_15x15.pic") ]


  forM_ sprites $ \(name, file) -> do
    game <- readIORef state
    sprite <- loadSprite file
    writeIORef state $ registrySprite game name sprite

  putStrLn "Sprites loaded..."

initObjects :: IORef GameState -> IO ()
initObjects state = do
  game <- readIORef state
  let objs = [
        createHero (4*cellSize, 0*cellSize),
        createNormalTank (0*cellSize, 12*cellSize) ]
  writeIORef state $ registryObjects game objs
  putStrLn "Objects loaded..."

renderObject :: GameState -> Entity -> IO ()
renderObject game obj
  | isBullet obj = do
    let dir = dir2rot $ direction obj
    drawSpriteEx dir sprite rect
  | isTank obj = do
    let dir = dir2rot $ direction obj
    drawSpriteEx dir sprite rect
    let effects = spritesEffects obj
    when (not (null effects)) $ do -- рисуем эффекты
      let Just sprite = lookup (head (spritesEffects obj)) $ sprites game
      drawSprite sprite rect
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
    drawSprite sprite rect
    where
      objRect@((x, y), (w, h)) = getRect obj
      rect = ((x+borderSize, y+borderSize), (w, h))
      Just sprite = lookup (getSprite obj) (sprites game)

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
  when (health standart > 1) $ do -- game over
    currentColor $= Color4 0.0 0.0 0.0 0.7
    renderPrimitive Quads $ do
      vertex $ Vertex3 (i2f borderSize) (i2f borderSize) 0
      vertex $ Vertex3 (i2f borderSize) (i2f (borderSize+screenHeight)) 0
      vertex $ Vertex3 (i2f (borderSize+screenWidth)) (i2f (borderSize+screenHeight)) 0
      vertex $ Vertex3 (i2f (borderSize+screenWidth)) (i2f borderSize) 0
    let Just sprite@(Sprite w h _) = lookup "gameover" $ sprites game
    let rect = ((borderSize + ((screenWidth-w) `div` 2), borderSize + ((screenHeight-h) `div` 2)), (w, h))
    currentColor $= Color4 1.0 1.0 1.0 1.0
    drawSprite sprite rect

  when (pause game) $ do -- пауза
    currentColor $= Color4 0.0 0.0 0.0 0.8
    renderPrimitive Quads $ do
      vertex $ Vertex3 (i2f borderSize) (i2f borderSize) 0
      vertex $ Vertex3 (i2f borderSize) (i2f (borderSize+screenHeight)) 0
      vertex $ Vertex3 (i2f (borderSize+screenWidth)) (i2f (borderSize+screenHeight)) 0
      vertex $ Vertex3 (i2f (borderSize+screenWidth)) (i2f borderSize) 0
    let Just sprite@(Sprite w h _) = lookup "pause" $ sprites game
    let rect = ((borderSize + ((screenWidth-w) `div` 2), borderSize + ((screenHeight-h) `div` 2)), (w, h))
    currentColor $= Color4 1.0 1.0 1.0 1.0
    drawSprite sprite rect
  --preservingMatrix $ do
  --  translate $ Vector3 (i2f 100) 0.0 0.0
  --  renderString Fixed8By13 . ("FPS: " ++) $ show (fps game)
  -- putStrLn $ ("FPS: " ++) $ show (fps game)
  displayInfoPanel state
  flush

displayNumber :: IORef GameState -> Int -> [(Int, Int)] -> IO ()
displayNumber state n rect = do
  game <- readIORef state
  let Just sprite@(Sprite w h _) = lookup "numbers" $ sprites game
      dx = (1.0 :: GLdouble) / (i2d w)
      dy = (1.0 :: GLdouble) / (i2d h)
      dn = i2d n
      x = dx * 16.0 * dn
      y = 0.0 :: GLdouble
      trect = [(x, y), (x, y + dy*16.0), (x + dx*16.0, y + dy*16.0), (x + dx*16.0, y)]
  drawSpriteEx' rect trect sprite

displayInfoPanel :: IORef GameState -> IO ()
displayInfoPanel state = do
  game <- readIORef state
  let enemies = enemyTanks game
      Just sprite = lookup "icon" $ sprites game
      x = screenWidth + 32 + 16
      y = screenHeight - 16

  forM_ [0 .. (enemies-1)] $ \i -> do
    let dx = i `mod` 2
        dy = i `div` 2
    drawSprite sprite ((x + dx*16 -1+dx*2, y - dy*17), (16, 16))

  -- жизни игрока
  let Just sprite = lookup "life" $ sprites game
  drawSprite sprite ((x, screenHeight `div` 2 - 32), (32, 32))
  let y = screenHeight `div` 2 - 32
      rect = [(x+16, y+2), (x+16, y+16), (x+32, y+16), (x+32, y+2)]
      lifes' = lifes game
  displayNumber state lifes' rect
  return ()

animTimer :: IORef GameState -> Int -> IO ()
animTimer state elapsed = do
  game <- readIORef state
  mapM_ (\(i, o) -> (onTimerCallback o) state elapsed i) $ objects game
  addTimerCallback elapsed (animTimer state elapsed)
  return ()

timerDt :: IORef GameState -> Int -> IO ()
timerDt state elapsed = do
  game <- readIORef state
  when (not (pause game)) $ do
    mapM_ (\(i, o) -> (onTimerCallback o) state elapsed i) $ objects game
  addTimerCallback elapsed (timerDt state elapsed)
  return ()

bulletsTimer :: IORef GameState -> Int -> IO ()
bulletsTimer state elapsed = do
  game <- readIORef state
  when (not (pause game)) $ do
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
  let key = event ch
  when (not (pause game)) $ do
    let standart = snd $ head $ filter (isStandart . snd) $ objects game
    when (health standart == 1) $ do
      if key /= Fire then do
        let oldKeys = delete MoveLeftward $ delete MoveRightward $ delete MoveForward $ delete MoveBackward $ (keys game)
        writeIORef state $ game { keys = oldKeys ++ [key] }
        else writeIORef state $ game { keys = (delete key (keys game)) ++ [key] }
  return ()

keyboardUp :: IORef GameState -> Char -> Position -> IO ()
keyboardUp state ch _ = do
  game <- readIORef state
  let key = event ch
  let standart = snd $ head $ filter (isStandart . snd) $ objects game
  when (health standart == 1) $ do
    if key == Pause then
      writeIORef state $ game { pause = not (pause game) }
      else
        writeIORef state $ game { keys = delete key (keys game) }
  return ()
