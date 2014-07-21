module HaskellRoguelike.LevelType where

    import Control.Monad.State
    import Control.Monad.Writer
    import Data.Array
    import Data.Map (Map)
    import qualified Data.Map as Map

    import HaskellRoguelike.State
    import HaskellRoguelike.Symbol
    import HaskellRoguelike.EntityType

    data Cell = 
        Cell { 
          baseSymbol :: Symbol,
          visible :: Bool,
          entities :: [EntityID]
        }
              deriving (Eq,Show)
          
    data Level = 
        Level {
          cells :: Array (Int, Int) Cell,
          entityMap :: Map EntityID (Entity Level),
          nextActors :: [EntityID],
          prevActors :: [EntityID],
          playerID :: Maybe EntityID
        }
           
    levelHeight :: Int
    levelHeight = 20
    
    levelWidth :: Int
    levelWidth = 80

    xMax = levelWidth - 1
    yMax = levelHeight - 1

    blankLevel = let c = Cell Blank True [] 
                 in Level {
                          cells = array ((0,0), (xMax,yMax)) 
                                  [(p,c) | p <- range ((0,0), (xMax,yMax))],
                          entityMap = Map.empty,
                          nextActors = [],
                          prevActors = [],
                          playerID = Nothing
                        }
              
    symbolAt :: Level -> (Int, Int) -> Symbol
    symbolAt l p = let c = (cells l) ! p
                   in if visible c then
                          cellSymbol c (entityMap l)
                      else
                          Blank

    getCell :: (Int,Int) -> RoguelikeM Level Cell
    getCell p = do l <- get
                   return ((cells l) ! p)

    setCell :: (Int,Int) -> Cell -> RoguelikeM Level ()
    setCell p c = do l <- get
                     put l{cells = (cells l)//[(p,c)]}

    tellUpdateCell :: (Int,Int) -> RoguelikeM Level ()
    tellUpdateCell p = 
        do l <- get
           case playerID l of
             Just pid -> 
                 do playerPos <- return (position $ (Map.!) (entityMap l) (pid))
                    los <- hasLOS playerPos p
                    case los of
                      True -> tell [UpdateCell p (symbolAt l p)]
                      False -> return ()

    tellDrawLevel :: RoguelikeM Level ()
    tellDrawLevel = 
        do l <- get
           case playerID l of
             Just pid -> 
                 do doFOV (position $ (Map.!) (entityMap l) (pid))
                    l' <- get
                    xs <- return $ assocs (cells l')
                    ys <- return $ map (\(p,c) -> (p,symbolAt l' p)) xs
                    tell [DrawLevel 
                          (array ((0,0), (levelWidth-1,levelHeight-1)) ys)]
             Nothing -> return ()
                       
                   
    cellSymbol :: Cell -> Map EntityID (Entity Level) -> Symbol
    cellSymbol c m = 
        case entities c of {
                          [] -> baseSymbol c;
                          e:es -> entitySymbol ((Map.!) m e)
                        }

    isClear :: Cell -> Map EntityID (Entity Level) -> Bool
    isClear c m = 
        let hasLargeEntity = 
                foldl 
                (\b -> \eid -> b || ((entitySize $ (Map.!) m eid) == Large))
                False (entities c)
            in
              if hasLargeEntity then
                  False
              else
                  case baseSymbol c of
                    Blank -> True
                    Floor -> True
                    _ -> False

    blocksLOS :: Cell -> Bool
    blocksLOS c = case baseSymbol c of
                    Blank -> True
                    Floor -> True
                    _ -> False
                    
    bresenhamLine :: (Int,Int) -> (Int,Int) -> (Cell -> (Bool,Cell)) -> RoguelikeM Level Bool
    bresenhamLine (x0,y0) (x1,y1) f = loop x0 y0 steps 
        where dx = abs (x1 - x0)
              dy = abs (y1 - y0)
              sx = signum (x1 - x0)
              sy = signum (y1 - y0)
              steps = if dy > dx then 
                          dy - dx
                      else
                          dx - dy
              loop x y n = let (x',y',n') =
                                     if dy > dx then
                                         if n <= 0 then
                                             (x+sx, y+sy, n+steps)
                                         else
                                             (x, y+sy, n-dx)
                                     else
                                         if n <= 0 then
                                             (x+sx, y+sy, n+steps)
                                         else
                                             (x+sx, y, n-dy)           
                             in do c <- getCell (x,y)
                                   (b,c') <- return $ f c
                                   setCell (x,y) c'
                                   case (x == x1) && (y == y1) of
                                     True -> return True
                                     False -> case b of
                                                True -> loop x' y' n'
                                                False -> return False
          
    hasLOS :: (Int,Int) -> (Int,Int) -> RoguelikeM Level Bool
    hasLOS p0 p1 = bresenhamLine p0 p1 (\c -> (blocksLOS c, c))
      
    doFOV :: (Int,Int) -> RoguelikeM Level ()
    doFOV p0 = 
        let bl = (\p1 -> 
                  bresenhamLine p0 p1 (\c -> (blocksLOS c, c{visible=True})))
        in
          do 
            forM_ (range ((0,0), (xMax,yMax))) (\p -> getCell p >>= (\c -> setCell p c{visible=False}))
            forM_ (range ((0,0), (xMax,0))) bl
            forM_ (range ((0,0), (0,yMax))) bl
            forM_ (range ((0,yMax), (xMax,yMax))) bl
            forM_ (range ((xMax,0), (xMax,yMax))) bl