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
          baseSymbol :: TerrainSymbol,
          visible :: Bool,
          explored :: Bool,
          entities :: [EntityID]
        }
        deriving (Eq,Show)

    class CellGrid c where
        getCell :: c -> (Int,Int) -> Maybe Cell
        setCell :: c -> (Int,Int) -> Cell -> c
          
    data Level = 
        Level {
          cells :: Array (Int, Int) Cell,
          entityMap :: Map EntityID (Entity Level),
          nextActors :: [EntityID],
          prevActors :: [EntityID],
          playerID :: Maybe EntityID
        }

    instance CellGrid Level where
        getCell l (x,y) = let ((x1,y1),(x2,y2)) = bounds (cells l) in
                      if (x < x1) || (y < y1) || (x > x2) || (y > y2) then
                          Nothing
                      else
                          Just (cells l ! (x,y))
        setCell l p c = l{cells = cells l//[(p,c)]}
           
    levelHeight :: Int
    levelHeight = 20
    levelWidth :: Int
    levelWidth = 80

    xMax :: Int
    xMax = levelWidth - 1
    yMax :: Int
    yMax = levelHeight - 1

    blankCell :: Cell
    blankCell = Cell BlankTerrain False False [] 

    blankLevel :: Level
    blankLevel = Level {
                   cells = array ((0,0), (xMax,yMax)) 
                           [(p,blankCell) | p <- range ((0,0), (xMax,yMax))],
                   entityMap = Map.empty,
                   nextActors = [],
                   prevActors = [],
                   playerID = Nothing
                 }
              
    symbolAt :: Level -> (Int, Int) -> Symbol
    symbolAt l p = let c = cells l ! p
                   in cellSymbol c (entityMap l)

    getCellM :: CellGrid c =>  (Int,Int) -> RoguelikeM c (Maybe Cell)
    getCellM p = do l <- get
                    return $ getCell l p

    setCellM :: CellGrid c => (Int,Int) -> Cell -> RoguelikeM c ()
    setCellM p c = do l <- get
                      put $ setCell l p c

    tellUpdateCell :: (Int,Int) -> RoguelikeM Level ()
    tellUpdateCell p = 
        do l <- get
           case playerID l of
             Just pid -> 
                 do let playerPos = position $ (Map.!) (entityMap l) pid
                    los <- hasLOS playerPos p
                    when los $ tell [UpdateCell p (symbolAt l p)]
             Nothing  -> return ()


    tellDrawLevel :: RoguelikeM Level ()
    tellDrawLevel = 
        do l <- get
           case playerID l of
             Just pid -> 
                 do doFOV (position $ (Map.!) (entityMap l) pid)
                    l' <- get
                    let xs = assocs (cells l')
                    let ys = map (\(p,_) -> (p,symbolAt l' p)) xs
                    tell [DrawLevel 
                          (array ((0,0), (xMax,yMax)) ys)]
             Nothing -> return ()
                       
                   
    cellSymbol :: Cell -> Map EntityID (Entity Level) -> Symbol
    cellSymbol c m
        | visible c =
            case entities c of
              [] -> Visible (Left (baseSymbol c));
              e:_ -> Visible (Right (entitySymbol ((Map.!) m e)))
        | explored c = Explored (baseSymbol c)
        | otherwise = Unexplored

    isClear :: Cell -> Map EntityID (Entity Level) -> Bool
    isClear c m = 
        let hasLargeEntity = 
                foldl 
                (\b eid -> b || (entitySize ((Map.!) m eid) == Large))
                False (entities c)
            in
              (not hasLargeEntity &&
               (case baseSymbol c of
                  BlankTerrain -> True
                  Floor -> True
                  _ -> False))

    blocksLOS :: Cell -> Bool
    blocksLOS c = case baseSymbol c of
                    BlankTerrain -> True
                    Floor -> True
                    _ -> False
                    
    bresenhamLine :: CellGrid c => 
                     (Int,Int) -> (Int,Int) -> (Cell -> (Bool,Cell)) -> RoguelikeM c Bool
    bresenhamLine (x0,y0) (x1,y1) f = loop x0 y0 steps 
        where dx = abs (x1 - x0)
              dy = abs (y1 - y0)
              sx = signum (x1 - x0)
              sy = signum (y1 - y0)
              steps = if dy > dx then 
                          dy - dx
                      else
                          dx - dy
              loop x y n = let (x',y',n') 
                                   | dy > dx =
                                         if n <= 0 then
                                             (x+sx, y+sy, n+steps)
                                         else
                                             (x, y+sy, n-dx)
                                    | n <= 0 = (x+sx, y+sy, n+steps)
                                    | otherwise = (x+sx, y, n-dy)           
                             in do mc <- getCellM (x,y)
                                   case mc of
                                     Nothing -> return False
                                     Just c -> do (b,c') <- return $ f c
                                                  setCellM (x,y) c'
                                                  if (x == x1) && (y == y1) then
                                                      return True
                                                  else 
                                                      if b then
                                                          loop x' y' n'
                                                      else 
                                                          return False
          
    hasLOS :: (Int,Int) -> (Int,Int) -> RoguelikeM Level Bool
    hasLOS p0 p1 = bresenhamLine p0 p1 (\c -> (blocksLOS c, c))
      
    doFOV :: (Int,Int) -> RoguelikeM Level ()
    doFOV p0 = 
        let bl p1 = bresenhamLine p0 p1 
                    (\c -> (blocksLOS c, 
                                      c{visible=True, explored=True}))
        in
          do 
            forM_ (range ((0,0), (xMax,yMax))) 
                      (\p -> do mc <- getCellM p
                                case mc of
                                  Nothing -> return ()
                                  Just c -> setCellM p c{visible=False})
            forM_ (range ((0,0), (xMax,0))) bl
            forM_ (range ((0,0), (0,yMax))) bl
            forM_ (range ((0,yMax), (xMax,yMax))) bl
            forM_ (range ((xMax,0), (xMax,yMax))) bl
