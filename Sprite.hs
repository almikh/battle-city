{-# LANGUAGE TupleSections #-}
module Sprite where

import Control.Monad
import Graphics.UI.GLUT
import Graphics.Rendering.OpenGL

import Control.Monad (when)
import System.Exit (exitFailure)
import System.IO (withBinaryFile, IOMode(ReadMode), openBinaryFile, hGetBuf)
import Foreign.Marshal.Alloc (allocaBytes)

import qualified Data.List as L
import Data.Text

data ColorType =
  IRGBA Int Int Int Int {- r, g, b, a \in [0, 255] -}
  | FRGBA GLfloat GLfloat GLfloat GLfloat {- r, g, b, a \in [0.0, 1.0] -}

data RotationType = CLOCKWISE | ANTICLOCKWISE | VERTICAL_FLIP | ORIGINAL

data Sprite = Sprite {
  width :: Int,
  height :: Int,
  handle :: TextureObject
}

type TRect = ((Int, Int), (Int, Int))

instance Show ColorType where

dir2rot :: (Int, Int) -> RotationType
dir2rot (0, -1) = VERTICAL_FLIP
dir2rot (1, 0) = CLOCKWISE
dir2rot (-1, 0) = ANTICLOCKWISE
dir2rot _ = ORIGINAL

loadSprite :: FilePath -> IO Sprite
loadSprite file = do
  -- putStrLn $ "loading " ++ file
  let [_, w, h, _] = split (\c -> c=='.' || c=='_' || c=='x') $ pack file
  let width = read $ unpack w
      height = read $ unpack h
  hTex <- withBinaryFile file ReadMode $ \h -> do
    let bytes = width * height * 4
    allocaBytes bytes $ \pixels -> do
      bytes' <- hGetBuf h pixels bytes
      when (bytes' /= bytes) exitFailure
      [tex] <- genObjectNames 1
      texture Texture2D $= Enabled
      textureBinding Texture2D $= Just tex
      -- textureFilter Texture2D $= ((Nearest, Just Nearest), Nearest)
      textureFilter Texture2D $= ((Linear', Just Linear'), Linear')
      build2DMipmaps Texture2D RGBA' (fromIntegral width) (fromIntegral height)
        (PixelData RGBA UnsignedByte pixels)
      textureWrapMode Texture2D S $= (Repeated, ClampToEdge)
      textureWrapMode Texture2D T $= (Repeated, ClampToEdge)
      textureBinding Texture2D $= Nothing
      texture Texture2D $= Disabled
      return tex
  return $ (Sprite width height hTex)

i2f :: Int -> GLfloat
i2f = fromIntegral

i2d :: Int -> GLdouble
i2d = fromIntegral

getTextureCoord :: RotationType -> [(GLdouble, GLdouble)]
getTextureCoord ORIGINAL = [(0, 0), (0, 1), (1, 1), (1, 0)]
getTextureCoord CLOCKWISE = [(1, 0), (0, 0), (0, 1), (1, 1)]
getTextureCoord ANTICLOCKWISE = [(0, 1), (1, 1), (1, 0), (0, 0)]
getTextureCoord VERTICAL_FLIP = [(1, 1), (1, 0), (0, 0), (0, 1)]

-- рисует картинку в указанном прямоугольнике
drawSprite :: Sprite -> TRect -> IO ()
drawSprite sprite rect = do
  drawSpriteEx ORIGINAL sprite rect

drawSpriteEx :: RotationType -> Sprite -> TRect -> IO ()
drawSpriteEx rot sprite ((x, y), (w, h)) = do
  let imgCoords = [(x, y), (x, y+h), (x+w, y+h), (x+w, y)]
  let texCoords = getTextureCoord rot
  drawSpriteEx' imgCoords texCoords sprite

drawSpriteEx' :: [(Int, Int)] -> [(GLdouble, GLdouble)] -> Sprite -> IO ()
drawSpriteEx' imgCoords texCoords sprite = do
  texture Texture2D $= Enabled
  textureBinding Texture2D $= Just (handle sprite)
  color $ Color4 1 1 1 (1 :: GLdouble)
  renderPrimitive Quads $ do
    forM_ (L.zipWith (,) imgCoords texCoords) $ \((x, y), (tx, ty)) -> do
      texCoord $ TexCoord2 tx ty
      vertex $ Vertex2 (i2f x) (i2f y)
  textureBinding Texture2D $= Nothing
  texture Texture2D $= Disabled
