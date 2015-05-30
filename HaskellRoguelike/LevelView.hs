module HaskellRoguelike.LevelView where

    import Data.Array
    import qualified Data.Map as Map
    
    import HaskellRoguelike.Entity
    import HaskellRoguelike.Level
    import HaskellRoguelike.State
    import HaskellRoguelike.Symbol

    data LevelView = LevelView Level EntityID

    hasLOS :: LevelView -> (Int,Int) -> Bool
    hasLOS (LevelView l e) (x,y) =
        case position <$> Map.lookup e (entities l) of
          Nothing -> False
          Just (Left (x0,y0)) -> bresenhamLine l f (x0,y0) (x,y)
          Just (Right _) -> False
        where f l' p' = not $ blocksLOS l' p'
          
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
              loop x y n 
                  | (x == x1) && (y == y1) = True
                  | f l (x,y) = loop x' y' n'
                  | otherwise = False
                  where (x',y',n') 
                            | (n <= 0)  = (x+sx, y+sy, n+steps)
                            | (dy > dx) = (x,    y+sy, n-dx)
                            | otherwise = (x+sx, y,    n-dy)

    displayLevelView :: LevelView -> IO ()
    displayLevelView lv@(LevelView l _) =
        printSymbolArray $ fmap (cellSymbol l . visibleCell) ixArray
        where ixArray = (listArray levelBounds (range levelBounds))
              visibleCell p = ((cells l) ! p){visible = hasLOS lv p} 
                                
