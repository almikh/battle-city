{-# LANGUAGE TupleSections #-}
module PathFinder where

import Data.List
import Data.Maybe
import Data.Array.IArray
import Game

type Node = (Int, Int)
type Path = [Node]

diff = [(1 , 0), (0, 1), (-1, 0), (0, -1)]

findPath :: TGrid -> Node -> Node -> Path
findPath grid begin end
  | marked ! end <= 0 = []
  | otherwise = reverse $ backtrace marked end
    where marked = runWave grid begin

backtrace :: TGrid -> Node -> [Node]
backtrace grid lst@(lx, ly)
  | null neighbors = []
  | otherwise = (head neighbors) : (backtrace grid (head neighbors))
    where
      nextMark = grid ! lst - 1
      foldFunc acc (dx, dy) = let (nx, ny) = (lx + dx, ly + dy) in
            if (isValidInd grid nx ny) && (grid ! (nx, ny) == nextMark) then (nx, ny) : acc else acc
      neighbors = foldl foldFunc [] diff

runWave :: TGrid -> Node -> TGrid
runWave grid point = createWaves (grid // [(point, 1)]) [point] 2

createWaves :: TGrid -> [Node] -> Int -> TGrid
createWaves grid [] nwave = grid
createWaves grid nodes nwave = createWaves new wave (nwave+1)
  where
    wave = nextWave grid nodes
    new = fillWave grid wave nwave

fillWave :: TGrid -> [Node] -> Int -> TGrid
fillWave grid nodes nwave = grid // (map (, nwave) nodes)

nextWave :: TGrid -> [Node] -> [Node]
nextWave grid wave = foldl (\acc node -> acc `merge` (check grid node)) [] wave
  where
    merge l1 l2 = foldl mergeFoldFunc l1 l2
    mergeFoldFunc xs y = if isJust (find (==y) xs) then xs else (y : xs)

check :: TGrid -> Node -> [Node]
check grid (x, y) = foldl foldFunc [] diff
  where foldFunc acc (dx, dy) = let (nx, ny) = (x + dx, y + dy) in
          if (isValidInd grid nx ny) && (grid ! (nx, ny) == 0) then (nx, ny) : acc
            else acc
