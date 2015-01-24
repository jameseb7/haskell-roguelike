module HaskellRoguelike.Entity.Context where

    import Control.Monad
    import Control.Monad.State
    import Data.Ix

    import HaskellRoguelike.Level.Cell
    import HaskellRoguelike.State

    class EntityContext c where
        getCell :: (Int,Int) -> c -> Maybe Cell
        setCell :: (Int,Int) -> Cell -> c -> c
        contextSize :: c -> (Int,Int) 
        updateCell :: (Cell -> Cell) -> (Int,Int) -> c -> c

        updateCell f p ec = case getCell p ec of
                              Nothing -> ec
                              Just c  -> setCell p (f c) ec

    updateCellM :: EntityContext c => 
                   (Cell -> Cell) 
                -> (Int,Int) 
                -> RoguelikeM c ()
    updateCellM f p = state (\ ec ->  ((), updateCell f p ec))

    getCellM :: EntityContext c =>  (Int,Int) -> RoguelikeM c (Maybe Cell)
    getCellM p = get >>= ( \ ec -> return $ getCell p ec)

    setCellM :: EntityContext c => (Int,Int) -> Cell -> RoguelikeM c ()
    setCellM p c = get >>= (\ ec -> put $ setCell p c ec)

    bresenhamLine :: EntityContext c => 
                     (Int,Int) 
                  -> (Int,Int) 
                  -> (Cell -> (Bool,Cell)) 
                  -> c 
                  -> (Bool, c)
    bresenhamLine (x0,y0) (x1,y1) f = loop x0 y0 steps
        where dx = abs (x1 - x0)
              dy = abs (y1 - y0)
              sx = signum (x1 - x0)
              sy = signum (y1 - y0)
              steps = if dy > dx then dy - dx else dx - dy
              loop x y n ec = maybe (False, ec)                       
                              (\ c -> let (b,c') = f c
                                          ec' = setCell (x,y) c' ec
                                      in if (x == x1) && (y == y1) 
                                         then (True, ec')
                                         else if b 
                                              then loop x' y' n' ec'
                                              else (False, ec'))
                              (getCell (x,y) ec)
                  where (x',y',n') 
                            | dy > dx   = if n <= 0 
                                          then (x+sx, y+sy, n+steps)
                                          else (x, y+sy, n-dx)
                            | n <= 0    = (x+sx, y+sy, n+steps)
                            | otherwise = (x+sx, y, n-dy)           

    hasLOS :: EntityContext c => (Int,Int) -> (Int,Int) -> c -> (Bool, c)
    hasLOS p0 p1 = bresenhamLine p0 p1 (\ c -> (blocksLOS c, c))
      
    doFOV :: EntityContext c => (Int,Int) -> RoguelikeM c ()
    doFOV p0 = do ec <- get
                  let (sx, sy) = contextSize ec
                  let (xMax, yMax) = (sx-1, sy-1)
                  forM_ (range ((0,0), (xMax,yMax))) clear
                  forM_ (range ((0,0), (xMax,0))) bl
                  forM_ (range ((0,0), (0,yMax))) bl
                  forM_ (range ((0,yMax), (xMax,yMax))) bl
                  forM_ (range ((xMax,0), (xMax,yMax))) bl
          where bl p1 = state $ bresenhamLine p0 p1 
                        (\c -> (blocksLOS c, c{visible=True, explored=True}))
                clear p = getCellM p >>= 
                          maybe (return ()) (\ c -> setCellM p c{visible=False})