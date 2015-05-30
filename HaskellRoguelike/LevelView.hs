module HaskellRoguelike.LevelView where

import HaskellRoguelike.Entity
import HaskellRoguelike.Level

data LevelView = LevelView Level EntityID

hasLOS :: LevelView -> Bool
hasLOS (LevelView l e) = True

bresenhamLine :: Level
              -> (Level -> (Int,Int) -> Bool)
              -> (Int,Int)
              -> (Int,Int)
              -> Bool
bresenhamLine l f (x0,y0) (x1,y1)
  | not (inLevelBounds (x0,y0)) = error $ "bresenhamLine: (x0,y0) not in level bounds: " ++ show (x0,y0)
  | not (inLevelBounds (x1,y1)) = error $ "bresenhamLine: (x0,y0) not in level bounds: " ++ show (x1,y1)
  | otherwise = loop x0 y0 steps 
  where dx = abs (x1 - x0)
        dy = abs (y1 - y0)
        sx = signum (x1 - x0)
        sy = signum (y1 - y0)
        steps = if dy > dx then dy - dx else dx - dy
        loop x y n =
          | (x == x1) && (y == y1) = False
          | f l = loop x' y' n'
          | otherwise = False
          where (x',y',n') 
                  | (n <= 0)  = (x+sx, y+sy, n+steps)
                  | (dy > dx) = (x,    y+sy, n-dx)
                  | otherwise = (x+sx, y,    n-dy)
                                
