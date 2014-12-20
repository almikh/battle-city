module Sprite where

import Control.Monad
import Graphics.UI.GLUT
import Graphics.Rendering.OpenGL

data ColorType =
  IRGBA Int Int Int Int {- r, g, b, a \in [0, 255] -}
  | FRGBA GLfloat GLfloat GLfloat GLfloat {- r, g, b, a \in [0.0, 1.0] -}

data RotationType = CLOCKWISE | ANTICLOCKWISE | VERTICAL_FLIP | ORIGINAL

data Sprite = Sprite {
  width :: Int,
  height :: Int,
  pixels :: [ColorType]
}

type TRect = ((Int, Int), (Int, Int))

instance Show ColorType where
  show (IRGBA r g b a) = (show r) ++ " " ++ (show g) ++ " " ++ (show b) ++ " " ++ (show a)
  show (FRGBA r g b a) = (show r) ++ " " ++ (show g) ++ " " ++ (show b) ++ " " ++ (show a)

dir2rot :: (Int, Int) -> RotationType
dir2rot (0, -1) = VERTICAL_FLIP
dir2rot (1, 0) = CLOCKWISE
dir2rot (-1, 0) = ANTICLOCKWISE
dir2rot _ = ORIGINAL

transform :: [Int] -> [ColorType]
transform (r : g : b : a : others) = (IRGBA r g b a) : transform others
transform [] = []

loadSprite :: FilePath -> IO Sprite
loadSprite file = do
  contents <- readFile file
  let (w : h : pxs) = map read $ words contents
  return $ Sprite w h $ transform pxs

createSprite :: Int -> Int -> ColorType -> Sprite
createSprite w h color = Sprite w h pixels
  where pixels = take (w*h) (repeat color)

i2f :: Int -> GLfloat
i2f = fromIntegral

color4 :: ColorType -> Color4 GLfloat
color4 (IRGBA r g b a) =
  Color4 ((i2f r)/255.0) ((i2f g)/255.0) ((i2f b)/255.0) ((i2f a)/255.0)
color4 (FRGBA r g b a) =
  Color4 r g b a

-- использовать только внутри renderPrimitive!
drawPoint :: GLfloat -> GLfloat -> ColorType -> IO ()
drawPoint i j color = do
  currentColor $= color4 color
  vertex $ Vertex3 i j 0

collectPoint :: RotationType -> Int -> Int -> Double -> [(Double, Double)]
collectPoint VERTICAL_FLIP w h zoom = map (\i ->
  (fromIntegral (i `mod` w) * zoom, fromIntegral (h-(i `div` w)) * zoom)) [1 .. (w*h)]
collectPoint ANTICLOCKWISE w h zoom = foldl1 (++) $ take h $ iterate (map (\(x, y) ->
  (x-zoom, y))) [(fromIntegral w * zoom, fromIntegral x * zoom) | x <- [1 .. h]]
collectPoint CLOCKWISE w h zoom = foldl1 (++) $ take h $ iterate (map (\(x, y) ->
  (x+zoom, y))) [(zoom, fromIntegral x * zoom) | x <- [1 .. h]]
collectPoint _ w h zoom = map (\i -> (fromIntegral (i `mod` w) * zoom, fromIntegral (i `div` w) * zoom)) [1 .. (w*h)]

-- Usage: drawSprite x y sprite pixelZoom
drawSprite :: Sprite -> TRect -> IO ()
drawSprite sprite rect = drawSpriteEx ORIGINAL sprite rect

drawSpriteEx :: RotationType -> Sprite -> TRect -> IO ()
drawSpriteEx rot (Sprite w h pxs) ((x, y), (rw, rh)) = do
  let zoom = fromIntegral rw / fromIntegral w
  let inds = map (\(i, j)-> ((fromIntegral x) + i, (fromIntegral y) + j)) $ collectPoint rot w h zoom
  let draw = zipWith (\ind color -> (ind, color)) inds pxs
  pointSize $= realToFrac zoom
  renderPrimitive Points $ forM_ draw $ \((i, j), color) -> drawPoint (realToFrac i) (realToFrac j) color
  pointSize $= 1.0
